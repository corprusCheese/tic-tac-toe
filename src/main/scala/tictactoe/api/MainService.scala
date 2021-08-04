package tictactoe.api

import cats.data.Kleisli
import cats.effect.{ContextShift, IO, Timer}
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import org.http4s.{Request, Response}
import tictactoe.api.services.{FileService, HttpService}

import scala.concurrent.ExecutionContext

object MainService {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val tmr: Timer[IO]       = IO.timer(ExecutionContext.global)

  val api: Kleisli[IO, Request[IO], Response[IO]] = Router(
    "/" -> new HttpService[IO].getInstance(),
    "/" -> new FileService[IO].getInstance()
  ).orNotFound
}
