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


class ChatService(consumersListOfIors: Ref[IO, IorUserList]) extends Http4sDsl[IO] with AbstractService {

  private val chatService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / "start" =>
      val inStreamProcessor: Pipe[IO, WebSocketFrame, Unit] = stream => {
        def inputLeftStreamAsInit(stream: Stream[IO, WebSocketFrame]) = {
          val ior: Ior.Left[Stream[IO, WebSocketFrame]] = Ior.Left(stream)
          consumersListOfIors.update(ior::_)
        }

        def enqueueIorToList(ior: MyIor, stream: Stream[IO, WebSocketFrame], message: String) = {
          if (ior.left.get != stream)
            ior.right.get.enqueue1(Text(message))
          else
            ior.right.get.enqueue1(Text(s"You send the message!"))
        }

        def getMessage(name: String, message: String) = {
          s"$name: $message"
        }

        def setStream(stream: Stream[IO, WebSocketFrame])= {
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

        inputLeftStreamAsInit(stream).unsafeRunSync()
        setStream(stream)
      }

      val outStream: Stream[IO, WebSocketFrame] = {
        def inputRightQueueAsUpdate(queue: Queue[IO, WebSocketFrame], ior: MyIor): MyIor = {
          ior.right match {
            case None =>
              ior.putRight(queue)
            case _ => ior
          }
        }

        Stream
          .eval(Queue.unbounded[IO, WebSocketFrame])
          .evalMap(queue => consumersListOfIors.update(iorUserList => iorUserList.map(ior => inputRightQueueAsUpdate(queue, ior))).map(_=>queue))
          .flatMap(queue => queue.dequeue)
      }
      WebSocketBuilder[IO].build(outStream, inStreamProcessor)
  }

  override def getInstance(): HttpRoutes[IO] = chatService
}

object ChatService {

  type MyIor = Ior[Stream[IO, WebSocketFrame], Queue[IO, WebSocketFrame]]
  type IorUserList = List[MyIor]

  def apply(): IO[ChatService] = for {
    consumers <- Ref.of[IO, IorUserList](List.empty)
  } yield new ChatService(consumers)
}
