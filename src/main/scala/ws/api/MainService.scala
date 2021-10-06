package ws.api

import cats.data.Kleisli
import cats.effect.{ContextShift, IO, Timer}
import core.settings.ServiceSettings
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.server.middleware._
import org.http4s.{Request, Response}
import ws.api.services.{ChatService, WsService}

import scala.concurrent._

object MainService {
  val api: IO[Kleisli[IO, Request[IO], Response[IO]]] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    implicit val tmr: Timer[IO]       = IO.timer(ExecutionContext.global)

    for {
      chatService <- ChatService.apply[IO]()
      wsService   <- WsService.apply[IO]()
    } yield Router(
      "/chat" -> CORS(chatService.getInstance(), ServiceSettings.methodConfig),
      "/"     -> CORS(wsService.getInstance(), ServiceSettings.methodConfig)
    ).orNotFound
  }
}
