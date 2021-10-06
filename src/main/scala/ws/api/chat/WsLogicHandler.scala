package ws.api.chat

import cats.{Monad, MonadThrow}
import cats.data._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits.{
  catsSyntaxApplicativeId,
  catsSyntaxFlatMapOps,
  catsSyntaxOptionId,
  none,
  toFlatMapOps,
  toFunctorOps,
  toTraverseOps
}
import fs2.{Pipe, Stream}
import fs2.concurrent.Queue
import io.circe.parser.parse
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import ws.api.model._
import messages._
import responses._
import ws.api.services.ChatService.{ComplexIor, IorUserList}

import scala.concurrent.duration.DurationInt
import ws.api.model.json._

class WsLogicHandler[F[_]: Monad: Timer: Concurrent](consumerList: Ref[F, IorUserList[F]]) {
  private def inputLeftStreamAsInit(stream: Stream[F, WebSocketFrame]): F[Unit] =
    consumerList.update(Ior.Left(Ior.Right(stream)) :: _)

  private def inputRightQueueAsUpdate(
    queue: Queue[F, WebSocketFrame],
    ior: ComplexIor[F]
  ): ComplexIor[F] =
    ior.right match {
      case None => ior.putRight(queue)
      case _    => ior
    }

  private def predicateForFindByName(myIor: ComplexIor[F], name: String): Boolean =
    myIor.left match {
      case Some(ior) =>
        ior.left match {
          case Some(str) => str.trim == name.trim
          case _         => false
        }
      case _ => false
    }

  private def sendSimpleMessage(ior: ComplexIor[F], message: String): F[Unit] =
    ior.right match {
      case Some(queue) => queue.enqueue1(Text(responseToJsonString(createResponse(message))))
      case _           => println("error").pure[F]
    }

  private def isCurrentStream(
    ior: ComplexIor[F],
    stream: Stream[F, WebSocketFrame]
  ): Option[Boolean] =
    ior.left match {
      case Some(myIor) =>
        myIor.right match {
          case Some(value) => Some(value == stream)
          case _           => None
        }
      case _ => None
    }

  private def findIorByName(name: String): F[Option[ComplexIor[F]]] =
    consumerList.get
      .map(_.find(myIor => predicateForFindByName(myIor, name)))

  private def isIorByNameFounded(name: String): F[Boolean] =
    findIorByName(name).map {
      case Some(_) => true
      case None    => false
    }

  private def getTargetMessage(message: String): String =
    getPartsFromMessage(message) match {
      case (sender, command, name) if sender != "" && command != "" && name != "" =>
        val array = message.split(" ")
        sender + " " + targetMarkMessage + " " + array.slice(3, array.length).mkString(" ")
      case _ => ""
    }

  private def sendTargetMessage(
    ior: ComplexIor[F],
    iorFound: ComplexIor[F],
    message: String
  ): F[Unit] =
    if (iorFound == ior)
      sendSimpleMessage(ior, getTargetMessage(message))
    else
      emptyAction()

  private def sendMessageByIor(
    ior: ComplexIor[F],
    optionIor: Option[ComplexIor[F]],
    message: String
  ): F[Unit] =
    optionIor match {
      case Some(iorFound) => sendTargetMessage(ior, iorFound, message)
      case None           => emptyAction()
    }

  private def sendCommandMessage(ior: ComplexIor[F], name: String, message: String): F[Unit] =
    findIorByName(name)
      .map(option => sendMessageByIor(ior, option, message))
      .flatMap(_.map(_ => ()))

  private def getNameFromIor(myIor: ComplexIor[F]): String =
    myIor.left match {
      case Some(ior) =>
        ior.left match {
          case Some(name) => name.trim
          case _          => ""
        }
      case _ => ""
    }

  private def getAllUsers(ior: ComplexIor[F]): F[Unit] =
    for {
      list   <- consumerList.get.map(_.map(getNameFromIor))
      string = list.mkString(", ")
      res    <- sendSimpleMessage(ior, usersMarkMessage + " " + string)
    } yield res

  private def trySendTarget(ior: ComplexIor[F], message: String): F[Unit] =
    getPartsFromMessage(message) match {
      case (_, command, name) if command == "/w"               => sendCommandMessage(ior, name, message)
      case (_, command, name) if command == "/g" && name == "" => emptyAction()
      case _                                                   => sendSimpleMessage(ior, message)
    }

  private def getPartsFromMessage(message: String): (String, String, String) =
    message.split(" ").toList match {
      case senderMessage :: command :: name :: _ =>
        (senderMessage.dropRight(1).trim, command.trim, name.trim)
      case senderMessage :: command :: _ => (senderMessage.dropRight(1).trim, command.trim, "")
      case senderMessage :: _            => (senderMessage.dropRight(1).trim, "", "")
      case _                             => ("", "", "")
    }

  private def sendIfThereIsUser(ior: ComplexIor[F], bool: Boolean): F[Unit] =
    if (bool)
      sendSimpleMessage(ior, notifyMessageOfSending)
    else
      sendSimpleMessage(ior, notifyMessageOfWrongUser)

  private def sendSenderMessage(ior: ComplexIor[F], name: String): F[Unit] =
    isIorByNameFounded(name)
      .map(bool => sendIfThereIsUser(ior, bool))
      .flatMap(_.map(_ => ()))

  private def sendToSenderByCommand(ior: ComplexIor[F], command: String): F[Unit] =
    if (command == "/g")
      getAllUsers(ior)
    else
      sendSimpleMessage(ior, notifyMessageOfSending)

  private def trySendToSender(ior: ComplexIor[F], message: String): F[Unit] =
    getPartsFromMessage(message) match {
      case (_, command, name) if name == ""                    => sendToSenderByCommand(ior, command)
      case (_, command, name) if name != "" && command == "/w" => sendSenderMessage(ior, name)
      case (_, _, name) if name != ""                          => sendSimpleMessage(ior, notifyMessageOfSending)
      case _                                                   => emptyAction()
    }

  private def sendMessage(ior: ComplexIor[F], message: String, isCurrentStream: Boolean): F[Unit] =
    if (isCurrentStream)
      trySendToSender(ior, message)
    else
      trySendTarget(ior, message)

  private def enqueueIorToList(
    ior: ComplexIor[F],
    stream: Stream[F, WebSocketFrame],
    message: String
  ): F[Unit] =
    isCurrentStream(ior, stream) match {
      case Some(boolean: Boolean) => sendMessage(ior, message, boolean)
      case None                   => println("error").pure[F]
    }

  private def getMessage(name: String, message: String): String = {
    val trimName = name.replace("\n", "").trim
    s"$trimName: $message"
  }

  private def setNameToIor(str: String, stream: Stream[F, WebSocketFrame]): F[Unit] =
    consumerList.update(_.map { myIor =>
      myIor.left match {
        case Some(ior) =>
          ior.right match {
            case Some(myStream) =>
              if (myStream == stream) myIor.putLeft(Ior.Both(str, stream)) else myIor
          }
      }
    })

  private def enqueueMessage(stream: Stream[F, WebSocketFrame], message: String): F[Unit] =
    consumerList.get
      .flatMap(_.map(ior => enqueueIorToList(ior, stream, message)).sequence)
      .map(_ => ())

  private def setNameHandler(
    json: String,
    stream: Stream[F, WebSocketFrame]
  ): F[(String, Option[String])] =
    parse(json) match {
      case Right(json) =>
        for {
          o <- MonadThrow[F]
                .fromEither(json.as[SetName])
                .map(setName =>
                  setNameToIor(setName.name, stream).map(_ => (setName.name, none[String]))
                )
          res <- o
        } yield res

      case _ => ("", none[String]).pure[F]
    }

  private def setMessageHandler(name: String, json: String): F[(String, Option[String])] =
    parse(json) match {
      case Right(json) =>
        for {
          o <- MonadThrow[F]
                .fromEither(json.as[ChatRequest])
                .map({
                  case publishMessage: PublishMessage =>
                    (name, publishMessage.message.some).pure[F]
                  case sendPrivateMessage: SendPrivateMessage =>
                    (
                      name,
                      ("/w " + sendPrivateMessage.name + " " + sendPrivateMessage.message).some
                    ).pure[F]
                })
          res <- o
        } yield res
      case _ => (name, none[String]).pure[F]
    }

  private def setStream(stream: Stream[F, WebSocketFrame]): Stream[F, Unit] =
    stream
      .evalMapAccumulate("")({
        case (name, Text(json, _)) if name == "" =>
          setNameHandler(json, stream)
        case (name, Text(json, _)) =>
          setMessageHandler(name, json)
      })
      .collect({
        case (name, Some(message)) =>
          (name, message)
      })
      .evalMap({
        case (name, message) =>
          enqueueMessage(stream, getMessage(name, message))
      })

  val inStreamProcessor: F[Pipe[F, WebSocketFrame, Unit]] = {
    def getPipe: Pipe[F, WebSocketFrame, Unit] =
      stream =>
        Stream
          .eval(inputLeftStreamAsInit(stream))
          .flatMap(_ => setStream(stream))
    getPipe.pure[F]
  }

  val outStream: F[Stream[F, WebSocketFrame]] =
    Stream
      .eval(Queue.unbounded[F, WebSocketFrame])
      .evalMap(queue =>
        Timer[F].sleep(1.second) >>
          consumerList
            .update(iorUserList => iorUserList.map(ior => inputRightQueueAsUpdate(queue, ior)))
            .map(_ => queue)
      )
      .flatMap(queue => queue.dequeue)
      .pure[F]
}
