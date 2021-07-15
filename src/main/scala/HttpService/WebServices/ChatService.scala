package helloworld
package HttpService.WebServices

import helloworld.HttpService.Algebra.AbstractService
import helloworld.HttpService.Game.Logic._
import cats.effect.IO
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
import cats.data.Ior
import cats.effect.{Concurrent, Timer}
import cats.data.Ior
import cats.effect._
import cats.effect.concurrent._
import cats.implicits._

import scala.collection.mutable
import scala.language.implicitConversions
import cats.data.Ior
import helloworld.HttpService.WebServices.ChatService._

import scala.concurrent.duration.DurationInt


class ChatService[F[_]: Monad: Timer: Concurrent](consumersListOfIors: Ref[F, IorUserList[F]]) extends Http4sDsl[F] {
  private val chatService: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root / "start" =>

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
            case Some(str) => str == name
            case _ => false
          }
          case _ => false
        }

      def sendSimpleMessage(ior: MyIor[F], message: String): F[Unit] =
        ior.right match {
          case Some(queue) => queue.enqueue1(Text(message))
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

      def sendTargetMessage(ior: MyIor[F], iorFound: MyIor[F], message: String): F[Unit] =
        if (iorFound == ior)
          sendSimpleMessage(ior, message + " (targetMessage)")
        else
          sendSimpleMessage(ior, message + " (not targetMessage)")

      def sendMessageByIor(ior: MyIor[F], optionIor: Option[MyIor[F]], message: String): F[Unit] =
        optionIor match {
          case Some(iorFound) => sendTargetMessage(ior, iorFound, message)
          case None => println("sThere is no such user!").pure[F]
        }

      def sendCommandMessage(ior: MyIor[F], name: String, message: String): F[Unit] =
        findIorByName(name)
          .map(option => sendMessageByIor(ior, option, message))
          .flatMap(_.map(_ => ()))

      def getNameFromIor(myIor: MyIor[F]): String = {
          myIor.left match {
            case Some(ior) => ior.left match {
              case Some(name) => name
              case _ => ""
            }
            case _ => ""
          }
      }

      def getAllUsers(ior: MyIor[F]): F[Unit] = {
        for {
          list <- consumersListOfIors.get.map(_.map(getNameFromIor))
          string = list.mkString(", ")
          res <- sendSimpleMessage(ior, string)
        } yield res
      }

      def dontSend(): F[Unit] = println("don't send").pure[F]

      def trySendTarget(ior: MyIor[F], message: String): F[Unit] =
        message.split(" ").toList match {
          case senderMessage::command::name::_ if command == "/w" => sendCommandMessage(ior, name, message)
          case senderMessage::command::_ if command == "/g" => dontSend()
          case _ => sendSimpleMessage(ior, message)
        }

      def getPartsFromMessage(message: String): (String, String, String) =
        message.split(" ").toList match {
          case senderMessage::command::name::_ => (senderMessage.dropRight(1).trim, command, name)
          case senderMessage::command::_ => (senderMessage.dropRight(1).trim, command, "")
          case senderMessage::_ => (senderMessage.dropRight(1).trim, "", "")
          case _ => ("", "", "")
        }

      def sendIfThereIsUser(ior:MyIor[F], bool: Boolean): F[Unit] =
        if (bool)
          sendSimpleMessage(ior, s"You send the message!")
        else
          sendSimpleMessage(ior, s"There is no such user!")

      def sendSenderMessage(ior:MyIor[F], name: String): F[Unit] =
        isIorByNameFounded(name)
          .map(bool => sendIfThereIsUser(ior, bool))
          .flatMap(_.map(_ => ()))

      def trySendToSender(ior: MyIor[F], message: String): F[Unit] = {
        val parts = getPartsFromMessage(message)
        parts._3 match {
          case value if value == "" =>
            if (parts._2 == "/g")
              getAllUsers(ior)
            else
              sendSimpleMessage(ior, s"You send the message!")
          case value if value != "" => sendSenderMessage (ior, value)
        }
      }

      def sendMessage(ior: MyIor[F], message: String, isCurrentStream: Boolean): F[Unit] =
        if (isCurrentStream)
          trySendToSender(ior, message)
        else
          trySendTarget(ior, message)

      def enqueueIorToList(ior: MyIor[F], stream: Stream[F, WebSocketFrame], message: String): F[Unit] =
        isCurrentStream(ior, stream) match {
          case Some(boolean: Boolean) =>
            sendMessage(ior, message, boolean)
          case None =>
            println("error").pure[F]
        }

      def getMessage(name: String, message: String): String = s"$name: $message"

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

      def setStream(stream: Stream[F, WebSocketFrame]): Stream[F, Unit] =
        stream.evalMapAccumulate("")({
          case (name, frame@Text(str, _)) if name == "" =>
            setNameToIor(str, stream).map(_=>(str, none[String]))
          case (name, frame@Text(str, _)) =>
            (name, str.some).pure[F]
        }).collect({
          case (name, Some(message)) => (name, message)
        }).evalMap({
          case (name, message) => enqueueMessage(stream, getMessage(name, message))
        })

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
              consumersListOfIors.update(iorUserList => iorUserList.map(ior => inputRightQueueAsUpdate(queue, ior)))
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
}
