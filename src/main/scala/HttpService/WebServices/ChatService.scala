package helloworld
package HttpService.WebServices

import cats.data.Kleisli
import cats.effect.IO
import helloworld.HttpService.Game.DataGameHandler
import io.circe._
import io.circe.parser._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{Callback, HttpRoutes, Request, Response}
import io.circe.syntax.EncoderOps
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import helloworld.HttpService.Algebra.AbstractService
import helloworld.HttpService.Game.Logic._
import cats.effect.concurrent._
import cats.implicits._
import fs2.{Stream, _}
import fs2.concurrent._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import cats._
import cats.effect.{Concurrent, Timer}
import cats.effect._
import cats.effect.concurrent._
import cats.implicits._
import cats.data.Kleisli
import cats.effect.IO
import helloworld.HttpService.Game.DataGameHandler
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{Callback, HttpRoutes, Request, Response}
import helloworld.HttpService.Game.Game._
import helloworld.HttpService.Game.Logic.{cs, _}

import scala.collection.mutable
import scala.language.implicitConversions
import cats.data.Ior
import helloworld.HttpService.WebServices.ChatService._

import scala.concurrent.duration.DurationInt
import scala.util.Try


class ChatService[F[_]: Monad: Timer: Concurrent](consumersListOfIors: Ref[F, IorUserList[F]]) extends Http4sDsl[F] {
  private val chatService: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root / "start" =>

      def targetMarkMessage(): String = "(You):"
      def usersMarkMessage(): String = "users:"
      def notifyMessageOfSending(): String = s"You send the message!"
      def notifyMessageOfWrongUser(): String = s"There is no such user!"

      def dontSend(): F[Unit] = Monad[F].pure(())

      def inputLeftStreamAsInit(stream: Stream[F, WebSocketFrame]): F[Unit] =
        consumersListOfIors.update(Ior.Left(Ior.Right(stream))::_)

      def inputRightQueueAsUpdate(queue: Queue[F, WebSocketFrame], ior: MyIor[F]): MyIor[F] =
        ior.right match {
          case None => ior.putRight(queue)
          case _ => ior
        }

      def predicateForFindByName(myIor: MyIor[F], name:String): Boolean =
        myIor.left match {
          case Some(ior) => ior.left match {
            case Some(str) => str.trim == name.trim
            case _ => false
          }
          case _ => false
        }

      def createResponse(message: String): ChatResponse = {
        val array = message.split(" ")
        array.toList match {
          case sender::mark::_ if mark == targetMarkMessage() =>
            PrivateMessage(sender.trim, array.slice(2, array.length).mkString(" "))
          case usersMark::_ if usersMark == usersMarkMessage() =>
            MessageToMe(array.slice(1, array.length).mkString(" "))
          case sender::_ =>
            array.mkString(" ") match {
              case value if value == notifyMessageOfWrongUser() || value == notifyMessageOfSending()  =>
                MessageToMe(value)
              case _ =>
                PublicMessage(sender.dropRight(1).trim, array.slice(1, array.length).mkString(" "))
            }
          case _ =>
            ErrorResponse("Wrong formatted message")
        }
      }

      def responseToJsonString(response: ChatResponse): String = {
        response match {
          case value: PrivateMessage => value.asJson.toString()
          case value: PublicMessage => value.asJson.toString()
          case value: MessageToMe => value.asJson.toString()
          case value: ErrorResponse => value.asJson.toString()
        }
      }

      def sendSimpleMessage(ior: MyIor[F], message: String): F[Unit] =
        ior.right match {
          case Some(queue) => queue.enqueue1(Text(responseToJsonString(createResponse(message))))
          case _ => println("error").pure[F]
        }

      def isCurrentStream(ior: MyIor[F], stream: Stream[F, WebSocketFrame]): Option[Boolean] =
        ior.left match {
          case Some(myIor) => myIor.right match {
            case Some(value) => Some(value == stream)
            case _ => None
          }
          case _ => None
        }

      def findIorByName(name: String): F[Option[MyIor[F]]] =
        consumersListOfIors
          .get
          .map(_.find(myIor => predicateForFindByName(myIor, name)))

      def isIorByNameFounded(name: String): F[Boolean] =
        findIorByName(name).map {
          case Some(_) => true
          case None => false
        }

      def getTargetMessage(message: String): String =
        getPartsFromMessage(message) match {
          case (sender, command, name) if (sender!="" && command!="" && name!="") =>
            val array = message.split(" ")
            sender + " " + targetMarkMessage() + " " + array.slice(3, array.length).mkString(" ")
          case _ => ""
        }

      def sendTargetMessage(ior: MyIor[F], iorFound: MyIor[F], message: String): F[Unit] =
        if (iorFound == ior)
          sendSimpleMessage(ior, getTargetMessage(message))
        else
          dontSend()

      def sendMessageByIor(ior: MyIor[F], optionIor: Option[MyIor[F]], message: String): F[Unit] =
        optionIor match {
          case Some(iorFound) => sendTargetMessage(ior, iorFound, message)
          case None => dontSend()
        }

      def sendCommandMessage(ior: MyIor[F], name: String, message: String): F[Unit] =
        findIorByName(name)
          .map(option => sendMessageByIor(ior, option, message))
          .flatMap(_.map(_ => ()))

      def getNameFromIor(myIor: MyIor[F]): String =
          myIor.left match {
            case Some(ior) => ior.left match {
              case Some(name) => name.trim
              case _ => ""
            }
            case _ => ""
          }

      def getAllUsers(ior: MyIor[F]): F[Unit] =
        for {
          list <- consumersListOfIors.get.map(_.map(getNameFromIor))
          string = list.mkString(", ")
          res <- sendSimpleMessage(ior, usersMarkMessage() + " " + string)
        } yield res

      def trySendTarget(ior: MyIor[F], message: String): F[Unit] =
        getPartsFromMessage(message) match {
          case (senderMessage, command, name) if command == "/w" => sendCommandMessage(ior, name, message)
          case (senderMessage, command, name) if command == "/g" && name == "" => dontSend()
          case _ => sendSimpleMessage(ior, message)
        }

      def getPartsFromMessage(message: String): (String, String, String) =
        message.split(" ").toList match {
          case senderMessage::command::name::_ => (senderMessage.dropRight(1).trim, command.trim, name.trim)
          case senderMessage::command::_ => (senderMessage.dropRight(1).trim, command.trim, "")
          case senderMessage::_ => (senderMessage.dropRight(1).trim, "", "")
          case _ => ("", "", "")
        }



      def sendIfThereIsUser(ior: MyIor[F], bool: Boolean): F[Unit] =
        if (bool)
          sendSimpleMessage(ior, notifyMessageOfSending())
        else
          sendSimpleMessage(ior, notifyMessageOfWrongUser())

      def sendSenderMessage(ior:MyIor[F], name: String): F[Unit] =
        isIorByNameFounded(name)
          .map(bool => sendIfThereIsUser(ior, bool))
          .flatMap(_.map(_ => ()))

      def sendToSenderByCommand(ior: MyIor[F], command: String): F[Unit] =
        if (command == "/g")
          getAllUsers(ior)
        else
          sendSimpleMessage(ior, notifyMessageOfSending())

      def trySendToSender(ior: MyIor[F], message: String): F[Unit] =
        getPartsFromMessage(message) match {
          case (sender, command, name) if name == "" => sendToSenderByCommand(ior, command)
          case (sender, command, name) if name != "" && command == "/w" => sendSenderMessage(ior, name)
          case (sender, command, name) if name != ""  => sendSimpleMessage(ior, notifyMessageOfSending())
          case _ => dontSend()
        }

      def sendMessage(ior: MyIor[F], message: String, isCurrentStream: Boolean): F[Unit] =
        if (isCurrentStream)
          trySendToSender(ior, message)
        else
          trySendTarget(ior, message)

      def enqueueIorToList(ior: MyIor[F], stream: Stream[F, WebSocketFrame], message: String): F[Unit] =
        isCurrentStream(ior, stream) match {
          case Some(boolean: Boolean) => sendMessage(ior, message, boolean)
          case None => println("error").pure[F]
        }

      def getMessage(name: String, message: String): String = {
        val trimName = name.replace("\n","").trim
        s"$trimName: $message"
      }

      def setNameToIor(str: String, stream: Stream[F, WebSocketFrame]): F[Unit] =
        consumersListOfIors.update(_.map(myIor => {
          myIor.left match {
            case Some(ior) => ior.right match {
              case Some(myStream) => if (myStream == stream) myIor.putLeft(Ior.Both(str, stream)) else myIor
            }
          }
        }))

      def enqueueMessage(stream: Stream[F, WebSocketFrame], message: String): F[Unit] =
        consumersListOfIors
          .get
          .flatMap(_.map(ior => enqueueIorToList(ior, stream, message)).sequence)
          .map(_ => ())

      def setNameHandler(json:String, stream: Stream[F, WebSocketFrame]): F[(String, Option[String])] = {
        parse(json) match {
          case Right(json) =>
            for {
              o <- MonadThrow[F].fromEither(json.as[SetName]).map(setName => setNameToIor(setName.name, stream).map(_ => (setName.name, none[String])))
              res <- o
            } yield res

          case _ => ("", none[String]).pure[F]
        }
      }

      def setMessageHandler(name: String, json: String): F[(String, Option[String])] = {
        parse(json) match {
          case Right(json) =>
            for {
              o <- MonadThrow[F].fromEither(json.as[ChatRequest]).map({
                case publishMessage: PublishMessage => (name, publishMessage.message.some).pure[F]
                case sendPrivateMessage: SendPrivateMessage => (name, ("/w " + sendPrivateMessage.name + " " + sendPrivateMessage.message).some).pure[F]
              })
              res <- o
            } yield res
          case _ => (name, none[String]).pure[F]
        }
      }

      def setStream(stream: Stream[F, WebSocketFrame]): Stream[F, Unit] = {
        stream.evalMapAccumulate("")({
          case (name, frame@Text(json, _)) if name == "" =>
            setNameHandler(json, stream)
          case (name, frame@Text(json, _)) =>
            setMessageHandler(name, json)
        }).collect({
          case (name, Some(message)) =>
            (name, message)
        }).evalMap({
          case (name, message) =>
            enqueueMessage(stream, getMessage(name, message))
        })
      }

      val pipe: Pipe[F, WebSocketFrame, Unit] = stream =>
        Stream
          .eval(inputLeftStreamAsInit(stream))
          .flatMap(_=>setStream(stream))

      val inStreamProcessor: F[Pipe[F, WebSocketFrame, Unit]] = pipe.pure[F]

      val outStream: F[Stream[F, WebSocketFrame]] =
        Stream
          .eval(Queue.unbounded[F, WebSocketFrame])
          .evalMap(queue =>
            Timer[F].sleep(1.second) >>
              consumersListOfIors
                .update(iorUserList => iorUserList.map(ior => inputRightQueueAsUpdate(queue, ior)))
                .map(_=>queue))
          .flatMap(queue => queue.dequeue)
          .pure[F]

      for {
        in <- inStreamProcessor
        out <- outStream
        res <- WebSocketBuilder[F].build(out, in)
      } yield res
  }

  def getInstance(): HttpRoutes[F] = chatService
}

object ChatService {
  type MyIor[F[_]] = Ior[Ior[String, Stream[F, WebSocketFrame]], Queue[F, WebSocketFrame]]
  type IorUserList[F[_]] = List[MyIor[F]]

  def apply[F[_]: Concurrent: Timer: Monad](): F[ChatService[F]] = for {
    consumers <- Ref.of[F, IorUserList[F]](List.empty[MyIor[F]])
  } yield new ChatService(consumers)

  sealed trait ChatRequest
  final case class SetName(name: String) extends ChatRequest
  final case class PublishMessage(message: String) extends ChatRequest
  final case class SendPrivateMessage(name: String, message: String) extends ChatRequest

  sealed trait ChatResponse
  final case class PublicMessage(from: String, message: String) extends ChatResponse
  final case class PrivateMessage(from: String, message: String) extends ChatResponse
  final case class ErrorResponse(description: String) extends ChatResponse
  final case class MessageToMe(message: String) extends ChatResponse

  implicit val decodeSetName: Decoder[SetName] = (c: HCursor) => {
    for {
      req <- c.downField("request").as[String]
      name <- c.downField("payload").downField("name").as[String]
      setName = if (req == "set_name") name else ""
    } yield SetName(setName)
  }

  implicit val decodeChatRequest: Decoder[ChatRequest] = (c: HCursor) => {
    for {
      request <- c.downField("request").as[String]
      message <- c.downField("payload").downField("message").as[String]

      res = request match {
        case "publish_message" =>
          PublishMessage(message)
        case "send_private_message" =>
          val either = for {
            name <- c.downField("payload").downField("name").as[String]
          } yield name

          either match {
            case Left(value) => PublishMessage("")
            case Right(name) => SendPrivateMessage(name, message)
          }
        case _ =>
          PublishMessage("")
      }
    } yield res
  }

  implicit val decodePublishMessage: Decoder[PublishMessage] = (c: HCursor) => {
    for {
      request <- c.downField("request").as[String]
      message <- c.downField("payload").downField("message").as[String]
      res = request match {
        case "publish_message" =>
          PublishMessage(message)
      }
    } yield res
  }

  implicit val decodeSendPrivateMessage: Decoder[SendPrivateMessage] = (c: HCursor) => {
    for {
      request <- c.downField("request").as[String]
      name <- c.downField("payload").downField("name").as[String]
      message <- c.downField("payload").downField("message").as[String]
      res = request match {
        case "send_private_message" =>
          SendPrivateMessage(name, message)
      }
    } yield res
  }

  implicit val encodePublicMessage: Encoder[PublicMessage] = (a: PublicMessage) => Json.obj(
    ("response", "public_message".asJson),
    ("data", (Map("message" -> a.message.asJson, "from" -> a.from.asJson)).asJson)
  )

  implicit val encodePrivateMessage: Encoder[PrivateMessage] = (a: PrivateMessage) => Json.obj(
    ("response", "private_message".asJson),
    ("data", (Map("message" -> a.message.asJson, "from"->a.from.asJson)).asJson)
  )

  implicit val encodeErrorResponse: Encoder[ErrorResponse] = (a: ErrorResponse) => Json.obj(
    ("response", "error".asJson),
    ("data", (Map("message" -> a.description.asJson)).asJson)
  )

  implicit val encodeMessageToMe: Encoder[MessageToMe] = (a: MessageToMe) => Json.obj(
    ("response", "system_message".asJson),
    ("data", (Map("message" -> a.message.asJson)).asJson)
  )

}
