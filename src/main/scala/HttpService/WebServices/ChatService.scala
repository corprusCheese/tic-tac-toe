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

import scala.collection.mutable
import scala.language.implicitConversions


class ChatService(consumersList: Ref[IO, List[Queue[IO, WebSocketFrame]]]) extends Http4sDsl[IO] with AbstractService {

  private val chatService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / "start" =>
          val inStreamProcessor2: Pipe[IO, WebSocketFrame, Unit] = stream => {
            stream.mapAccumulate("")({
              case (name, frame@Text(str, _)) if name == "" =>
                (str, none[String])
              case (name, frame@Text(str, _)) =>
                (name, str.some)
            }).collect({
              case (name, Some(message)) => (name, message)
            }).evalMap({
              case (name, message) =>
                consumersList.get.flatMap(_.map(_.enqueue1(Text(s"$name: $message"))).sequence).map(_ => ())
            })
          }

          val outStream: Stream[IO, WebSocketFrame] = {
            Stream.eval(Queue.unbounded[IO, WebSocketFrame])
              .evalMap(queue => consumersList.update(queue :: _).map(_ => queue))
              .flatMap(queue => queue.dequeue)
          }
          WebSocketBuilder[IO].build(outStream, inStreamProcessor2)
  }

  override def getInstance(): HttpRoutes[IO] = chatService
}

object ChatService {

  type Name = String
  case class ChatMessage(name: Name, wsf: WebSocketFrame) {
    def get(): String = {
      s"$name: $wsf.data"
    }
  }

  def apply(): IO[ChatService] = for {
    consumers <- Ref.of[IO, List[Queue[IO, WebSocketFrame]]](List.empty)
  } yield new ChatService(consumers)
}
