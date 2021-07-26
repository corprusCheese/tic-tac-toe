package helloworld.api

import cats.data._
import cats.effect._
import org.http4s._
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.Router
import services.HttpService

import scala.concurrent._

object MainHttpService {
  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val tmr: Timer[IO]       = IO.timer(ExecutionContext.global)
  val api: Kleisli[IO, Request[IO], Response[IO]] = Router(
    "/" -> new HttpService[IO].getInstance()
  ).orNotFound
}
