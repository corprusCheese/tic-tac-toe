package helloworld.api

import cats.data.Kleisli
import cats.effect.{ContextShift, IO, Timer}
import helloworld.game.Logic._
import helloworld.api.services.{ChatService, FileService, HttpService, WsService}
import helloworld.Main.{contextShift, timer}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.{Request, Response}
import org.http4s.server.middleware._
import scala.concurrent._
import scala.concurrent.duration._

object MainService {

  val methodConfig: CORSConfig = CORSConfig(
    anyOrigin = true,
    anyMethod = true,
    allowCredentials = true,
    maxAge = 1.day.toSeconds
  )

  val api: IO[Kleisli[IO, Request[IO], Response[IO]]] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    implicit val tmr: Timer[IO]       = IO.timer(ExecutionContext.global)

    for {
      chatService <- ChatService.apply[IO]()
      wsService <- WsService.apply[IO]()
      fileService <- FileService.apply[IO]()
    } yield Router(
      "/chat" -> CORS(chatService.getInstance(), methodConfig),
      "/" -> CORS(wsService.getInstance(), methodConfig),
      "/" -> CORS(fileService.getInstance(), methodConfig),
      "/" -> CORS(new HttpService[IO].getInstance(), methodConfig)
    ).orNotFound

  }
}
