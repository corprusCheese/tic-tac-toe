package helloworld
package HttpService.WebServices

import helloworld.HttpService.Algebra.AbstractService
import helloworld.HttpService.Game.Logic._
import cats.effect.IO
import cats.effect.concurrent._
import cats.implicits._
import fs2._
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

      val pipe: Pipe[F, WebSocketFrame, Unit] = stream => {
        def inputLeftStreamAsInit(stream: Stream[F, WebSocketFrame]) = {
          val ior: Ior.Left[Stream[F, WebSocketFrame]] = Ior.Left(stream)
          consumersListOfIors.update(ior::_)
        }

        def enqueueIorToList(ior: MyIor[F], stream: Stream[F, WebSocketFrame], message: String) = {
          if (ior.left.get != stream)
            ior.right.get.enqueue1(Text(message))
          else
            ior.right.get.enqueue1(Text(s"You send the message!"))
        }

        def getMessage(name: String, message: String) = {
          s"$name: $message"
        }

        def setStream(stream: Stream[F, WebSocketFrame])= {
          stream.mapAccumulate("")({
            case (name, frame@Text(str, _)) if name == "" =>
              (str, none[String])
            case (name, frame@Text(str, _)) =>
              (name, str.some)
          }).collect({
            case (name, Some(message)) => (name, message)
          }).evalMap({
            case (name, message) =>
              consumersListOfIors
                .get
                .flatMap(_.map(ior => enqueueIorToList(ior,stream, getMessage(name, message))).sequence)
                .map(_ => ())
          })
        }

        //inputLeftStreamAsInit(stream) >> setStream(stream).pure[F]
        //stream.evalTap(_ => inputLeftStreamAsInit(stream)).through(setStream)
        Stream.eval(inputLeftStreamAsInit(stream)).flatMap(_=>setStream(stream))
      }

      val inStreamProcessor: F[Pipe[F, WebSocketFrame, Unit]] = pipe.pure[F]

      val outStream: F[Stream[F, WebSocketFrame]] = {
        def inputRightQueueAsUpdate(queue: Queue[F, WebSocketFrame], ior: MyIor[F]): MyIor[F] = {
          ior.right match {
            case None =>
              ior.putRight(queue)
            case _ => ior
          }
        }

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

  type MyIor[F[_]] = Ior[Stream[F, WebSocketFrame], Queue[F, WebSocketFrame]]
  type IorUserList[F[_]] = List[MyIor[F]]

  def apply[F[_]: Concurrent: Timer: Monad](): F[ChatService[F]] = for {
    consumers <- Ref.of[F, IorUserList[F]](List.empty[MyIor[F]])
  } yield new ChatService(consumers)
}
