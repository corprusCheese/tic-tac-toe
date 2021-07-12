package helloworld
package HttpService

import cats.data.Kleisli
import cats.effect.IO
import helloworld.HttpService.WebServices.{ChatService, FileService, HttpService, WsService}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{Request, Response}
import org.http4s.server.Router

object MainService {
  val api: IO[Kleisli[IO, Request[IO], Response[IO]]] = {
    ChatService.apply().map(chatService => {

      Router(
        "/" -> FileService.getInstance(),
        "/" -> WsService.getInstance(),
        "/" -> HttpService.getInstance(),
        "/chat" -> chatService.getInstance()
      ).orNotFound
    })
  }
}
