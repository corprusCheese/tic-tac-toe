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


class ChatService[F[_]: Monad: Timer: Concurrent](consumersListOfIors: Ref[F, IorUserList[F]]) extends Http4sDsl[F] {

  private val chatService: HttpRoutes[F] = HttpRoutes.of[F] {
    case req@GET -> Root / "start" =>

      def inputLeftStreamAsInit(stream: Stream[F, WebSocketFrame]): F[Unit] =
        consumersListOfIors.update(Ior.Left(Ior.Right(stream))::_)

      def predicateForFindByName(myIor: MyIor[F], name:String): Boolean = {
        myIor.left match {
          case Some(ior) => ior.left match {
            case Some(str) => str == name
            case _ => false
          }
          case _ => false
        }
      }

      def spellToOptionStream(optionMyIor: Option[MyIor[F]]): Option[Stream[F, WebSocketFrame]] = {
        optionMyIor match {
          case Some(myIor) => myIor.left match {
            case Some(ior) => ior.right match {
              case Some(stream) => Some(stream)
              case _ => None
            }
            case _ => None
          }
          case None => None
        }
      }

      def sendSimpleMessage(ior: MyIor[F], message: String):F[Unit] = {
        ior.right match {
          case Some(queue) => queue.enqueue1(Text(message))
          case _ => println("error").pure[F]
        }
      }

      def sendTargetMessage(ior: MyIor[F], message: String, stream:Stream[F, WebSocketFrame], optionStream: Option[Stream[F, WebSocketFrame]]): F[Unit] = {
        optionStream match {
          case Some(streamFound) if streamFound == stream =>
            sendSimpleMessage(ior, message + "(targetMessage)")
          case None =>
            sendSimpleMessage(ior, s"There is no such user!")
        }
      }

      def isCurrentStream(ior: MyIor[F], stream: Stream[F, WebSocketFrame]): Option[Boolean] = {
        ior.left match {
          case Some(myIor) => myIor.right match {
            case Some(value) => Some(value == stream)
            case _ => None
          }
          case _ => None
        }
      }

      def trySendTarget(ior: MyIor[F], stream: Stream[F, WebSocketFrame], message: String): F[Unit] = {
        message.split(" ").toList match {
          case senderMessage::command::name::_ if command == "/w" =>
            consumersListOfIors
              .get
              .map(_.find(myIor => predicateForFindByName(myIor, name)))
              .map(spellToOptionStream)
              .map(optionStream => sendTargetMessage(ior, message, stream, optionStream))
          case _ => sendSimpleMessage(ior, message)
        }
      }

      def enqueueIorToList(ior: MyIor[F], stream: Stream[F, WebSocketFrame], message: String): F[Unit] = {
        isCurrentStream(ior, stream) match {
          case Some(value) =>
            if (value)
              sendSimpleMessage(ior, s"You send the message!")
            else
              trySendTarget(ior, stream, message)
          case None =>
            println("error").pure[F]
        }
      }

      def getMessage(name: String, message: String): String = {
        s"$name: $message"
      }

      def setNameToIor(str: String, stream: Stream[F, WebSocketFrame]): F[Unit] = {
        consumersListOfIors.update(_.map(myIor => {
          myIor.left match {
            case Some(ior) => ior.right match {
              case Some(myStream) =>
                if (myStream == stream)
                  myIor.putLeft(Ior.Both(str, stream))
                else myIor
            }
          }
        }))
      }

      def setStream(stream: Stream[F, WebSocketFrame]): Stream[F, Unit]= {
        val res = stream.evalMapAccumulate("")({
          case (name, frame@Text(str, _)) if name == "" =>
            setNameToIor(str, stream).map(_=>(str, none[String]))
          case (name, frame@Text(str, _)) =>
            (name, str.some).pure[F]
        }).collect({
          case (name, Some(message)) => (name, message)
        }).evalMap({
          case (name, message) =>
            consumersListOfIors
              .get
              .flatMap(_.map(ior => enqueueIorToList(ior, stream, getMessage(name, message))).sequence)
              .map(_ => ())
        })
        println(1)
        res
      }

      def inputRightQueueAsUpdate(queue: Queue[F, WebSocketFrame], ior: MyIor[F]): MyIor[F] = {
        println(2)
        ior.right match {
          case None =>
            ior.putRight(queue)
          case _ => ior
        }
      }

      val pipe: Pipe[F, WebSocketFrame, Unit] = stream => Stream.eval(inputLeftStreamAsInit(stream)).flatMap(_=>setStream(stream))
      val inStreamProcessor: F[Pipe[F, WebSocketFrame, Unit]] = pipe.pure[F]

      val outStream: F[Stream[F, WebSocketFrame]] = {
        Stream
          .eval(Queue.unbounded[F, WebSocketFrame])
          .evalMap(queue => consumersListOfIors.update(iorUserList => iorUserList.map(ior => inputRightQueueAsUpdate(queue, ior))).map(_=>queue))
          .flatMap(queue => queue.dequeue)
          .pure[F]
      }

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
