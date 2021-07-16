package helloworld.HttpService

import cats.data.Kleisli
import cats.effect.IO
import helloworld.game.Logic._
import helloworld.HttpService.WebServices.{ChatService, FileService, HttpService, WsService}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.{Request, Response}

object MainService {
  val api: IO[Kleisli[IO, Request[IO], Response[IO]]] = {
    ChatService.apply[IO]().map { chatService =>
      Router(
        "/"     -> FileService.getInstance(),
        "/"     -> WsService.getInstance(),
        "/"     -> HttpService.getInstance(),
        "/chat" -> chatService.getInstance()
      ).orNotFound
    }
  }
}
