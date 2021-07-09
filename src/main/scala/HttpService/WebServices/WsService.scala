package helloworld
package HttpService.WebServices

import cats.effect.IO
import fs2.concurrent.Queue
import fs2.{Pipe, Stream}
import io.circe.{Json, parser}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import org.http4s.{HttpRoutes, Response}
import cats.implicits._
import helloworld.HttpService.Game.DataGameHandler
import helloworld.HttpService.Game.Game._
import helloworld.HttpService.Game.Logic.{cs, _}
import io.circe.syntax.EncoderOps

import helloworld.HttpService.Algebra.AbstractService

import scala.concurrent.duration.DurationInt

object WsService extends Http4sDsl[IO] with AbstractService {

  private val socketService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "begin" => begin()
    case GET -> Root / "echo" => echo()
    case GET -> Root / "start" => start()
  }

  private def begin(): IO[Response[IO]] = {
    val toClient: Stream[IO, WebSocketFrame] = Stream.awakeEvery[IO](1.second).map(x => Text(s"x: $x"))
    val fromClient: Pipe[IO, WebSocketFrame, Unit] = _.evalMap({
      case Text(json, _) => IO.delay(println(s"$json"))
    })

    WebSocketBuilder[IO].build(toClient, fromClient)
  }

  private def echo(): IO[Response[IO]] = {
    val qio = Queue.unbounded[IO, Option[WebSocketFrame]]

    val buildStreams: Queue[IO, Option[WebSocketFrame]] => (Stream[IO, WebSocketFrame], Pipe[IO, WebSocketFrame, Unit]) = queue => {
      val fromClient: Pipe[IO, WebSocketFrame, Unit] = in => in.map(_.some).through(queue.enqueue)
      val toClient: Stream[IO, WebSocketFrame] = queue.dequeue.unNoneTerminate

      (toClient, fromClient)
    }

    val buildSocket: (Stream[IO, WebSocketFrame], Pipe[IO, WebSocketFrame, Unit]) => IO[Response[IO]] = (to, from) =>
      WebSocketBuilder[IO].build(to, from)

    qio.map(buildStreams).flatMap(buildSocket.tupled)
  }

  private def start(): IO[Response[IO]] = {
    val queue: IO[Queue[IO, Position]] = Queue
      .unbounded[IO, Position];

    def getStreams(queue: Queue[IO, Position]): (Stream[IO, WebSocketFrame], Pipe[IO, WebSocketFrame, Unit]) = {
      def parsePositionMessagePure(message: String): Either[Throwable, Position] = {
        for {
          json <- parser.parse(message)
          position <- json.as[Position]
        } yield position
      }

      def processInputMessage(message: WebSocketFrame): IO[Unit] = message match {
        case Text(str, _) =>
          IO.fromEither(parsePositionMessagePure(str))
            .flatMap(position => queue.enqueue1(position))
      }

      def makePipeFromClient(inputStream: Stream[IO, WebSocketFrame]): Stream[IO, Unit] = {
        inputStream.evalMap(processInputMessage)
      }

      def getStringBoard(board: Board): String = {
        DataGameHandler.getBoardAsMap(board).toString()
      }

      def makeFramesFromPositions(stream: Stream[IO, Position]): Stream[IO, WebSocketFrame] = {
        stream.map(position => {
          val state = logicService.addMarkAndGetNextState(
            board,
            turn,
            position
          )

          board = state._1
          turn = state._2
          WebSocketFrame.Text(getStringBoard(board))
        })
      }

      val fromClient: Pipe[IO, WebSocketFrame, Unit] = makePipeFromClient
      val toClient: Stream[IO, WebSocketFrame] = makeFramesFromPositions(queue.dequeue)

      (toClient, fromClient)
    }

    for {
      q <- queue
      streams = getStreams(q)
      build <- WebSocketBuilder[IO].build(streams._1, streams._2)
    } yield build
  }

  override def getInstance(): HttpRoutes[IO] = socketService
}
