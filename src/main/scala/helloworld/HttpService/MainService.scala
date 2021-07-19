package helloworld.HttpService

import cats.data.Kleisli
import cats.effect.{ContextShift, IO, Timer}
import helloworld.game.Logic._
import helloworld.HttpService.WebServices.{ChatService, FileService, HttpService, WsService}
import helloworld.Main.{contextShift, timer}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.{Request, Response}

import scala.concurrent.ExecutionContext

object MainService {
  val api: IO[Kleisli[IO, Request[IO], Response[IO]]] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    implicit val tmr: Timer[IO]       = IO.timer(ExecutionContext.global)
    for {
      chatService <- ChatService.apply[IO]()
      wsService <- WsService.apply[IO]()
      fileService <- FileService.apply[IO]()
      httpService <- HttpService.apply[IO]()
    } yield Router(
      "/chat" -> chatService.getInstance(),
      "/" -> wsService.getInstance(),
      "/" -> fileService.getInstance(),
      "/" -> httpService.getInstance()
    ).orNotFound

  }
}
