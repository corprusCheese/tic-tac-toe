package helloworld

import cats.effect._
import helloworld.HttpService.HttpService
import org.http4s.server.blaze._


object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    BlazeServerBuilder[IO]
      .bindHttp(5000, "0.0.0.0")
      .withHttpApp(HttpService.gameService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
