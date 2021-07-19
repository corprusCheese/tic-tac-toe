package helloworld.HttpService.WebServices

import cats.{Monad, MonadThrow}
import cats.effect.{Concurrent, ContextShift, Sync, Timer}
import cats.implicits._
import fs2.concurrent.Queue
import fs2.{Pipe, Stream}
import helloworld.game.Game._
import helloworld.algebra.AbstractService
import helloworld.game.DataGameHandler
import helloworld.game.Logic._
import io.circe.parser
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
    case GET -> Root / "start" => start()
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

  private def start(): F[Response[F]] = {
    val queue: F[Queue[F, Position]] = Queue
      .unbounded[F, Position];

    def getStreams(
      queue: Queue[F, Position]
    ): (Stream[F, WebSocketFrame], Pipe[F, WebSocketFrame, Unit]) = {
      def parsePositionMessagePure(message: String): Either[Throwable, Position] =
        for {
          json     <- parser.parse(message)
          position <- json.as[Position]
        } yield position

      def processInputMessage(message: WebSocketFrame): F[Unit] = message match {
        case Text(str, _) =>
          MonadThrow[F].fromEither(parsePositionMessagePure(str))
            .flatMap(position => queue.enqueue1(position))
      }

      def makePipeFromClient(inputStream: Stream[F, WebSocketFrame]): Stream[F, Unit] =
        inputStream.evalMap(processInputMessage)

      def getStringBoard(board: Board): String =
        DataGameHandler.getBoardAsMap(board).toString()

      def makeFramesFromPositions(stream: Stream[F, Position]): Stream[F, WebSocketFrame] =
        stream.map { position =>
          val state = logicService.addMarkAndGetNextState(
            board,
            turn,
            position
          )

          board = state._1
          turn = state._2
          WebSocketFrame.Text(getStringBoard(board))
        }

      val fromClient: Pipe[F, WebSocketFrame, Unit] = makePipeFromClient
      val toClient: Stream[F, WebSocketFrame]       = makeFramesFromPositions(queue.dequeue)

      (toClient, fromClient)
    }

    for {
      q       <- queue
      streams = getStreams(q)
      build   <- WebSocketBuilder[F].build(streams._1, streams._2)
    } yield build
  }

  override def getInstance(): HttpRoutes[F] = socketService
}

object WsService {
  def apply[F[_]: Concurrent: Timer: Monad: ContextShift](): F[WsService[F]] =
    new WsService[F].pure[F]
}