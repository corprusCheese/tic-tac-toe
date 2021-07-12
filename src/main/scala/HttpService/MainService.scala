package helloworld
package HttpService

import cats.data.Kleisli
import cats.effect.IO
import helloworld.HttpService.WebServices.{ChatService, FileService, HttpService, WsService}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.{Request, Response}
import org.http4s.server.Router

object MainService {
  val api: Kleisli[IO, Request[IO], Response[IO]] = Router(
    "/" -> FileService.getInstance(),
    "/" -> WsService.getInstance(),
    "/" -> HttpService.getInstance(),
    "/chat" -> ChatService.getInstance()
  ).orNotFound
}
