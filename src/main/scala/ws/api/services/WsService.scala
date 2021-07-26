package ws.api.services

import cats.Monad
import cats.effect.{Concurrent, ContextShift, Sync, Timer}
import cats.implicits._
import core.algebra.AbstractService
import fs2.concurrent.Queue
import fs2.{Pipe, Stream}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.http4s.{HttpRoutes, Response}

import scala.concurrent.duration.DurationInt

class WsService[F[_]: Monad: Timer: Concurrent: ContextShift] extends Http4sDsl[F] with AbstractService[F] {

  private val socketService: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "begin" => begin()
    case GET -> Root / "echo"  => echo()
  }

  private def begin(): F[Response[F]] = {
    val toClient: Stream[F, WebSocketFrame] =
      Stream.awakeEvery[F](1.second).map(x => Text(s"x: $x"))
    val fromClient: Pipe[F, WebSocketFrame, Unit] = _.evalMap({
      case Text(json, _) => Sync[F].delay(println(s"$json"))
    })

    WebSocketBuilder[F].build(toClient, fromClient)
  }

  private def echo(): F[Response[F]] = {
    val qio = Queue.unbounded[F, Option[WebSocketFrame]]

    val buildStreams: Queue[F, Option[WebSocketFrame]] => (
      Stream[F, WebSocketFrame],
      Pipe[F, WebSocketFrame, Unit]
    ) = queue => {
      val fromClient: Pipe[F, WebSocketFrame, Unit] = in => in.map(_.some).through(queue.enqueue)
      val toClient: Stream[F, WebSocketFrame]       = queue.dequeue.unNoneTerminate

      (toClient, fromClient)
    }

    val buildSocket
      : (Stream[F, WebSocketFrame], Pipe[F, WebSocketFrame, Unit]) => F[Response[F]] =
      (to, from) => WebSocketBuilder[F].build(to, from)

    qio.map(buildStreams).flatMap(buildSocket.tupled)
  }

  override def getInstance(): HttpRoutes[F] = socketService
}

object WsService {
  def apply[F[_]: Concurrent: Timer: Monad: ContextShift](): F[WsService[F]] =
    new WsService[F].pure[F]
}