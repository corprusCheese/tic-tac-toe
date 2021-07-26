package ws.api

import cats.Id
import cats.data.Kleisli
import cats.effect.{ContextShift, IO, Timer}
import core.settings.ServiceSettings
import tictactoe.api.game.Logic._
import ws.api.services.{ChatService, WsService}
import ws.Main.{contextShift, timer}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.{Request, Response}
import org.http4s.server.middleware._
import tictactoe.api.services.HttpService
import tictactoe.logs.LogManager

import scala.concurrent._
import scala.concurrent.duration._

object MainService {
  val api: IO[Kleisli[IO, Request[IO], Response[IO]]] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    implicit val tmr: Timer[IO]       = IO.timer(ExecutionContext.global)

    for {
      chatService <- ChatService.apply[IO]()
      wsService <- WsService.apply[IO]()
    } yield Router(
      "/chat" -> CORS(chatService.getInstance(), ServiceSettings.methodConfig),
      "/" -> CORS(wsService.getInstance(), ServiceSettings.methodConfig)
    ).orNotFound
  }
}
