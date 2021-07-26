package tictactoe

import cats.effect._
import core.settings.ServiceSettings.propertiesForServer
import org.http4s.server.blaze.BlazeServerBuilder
import tictactoe.api.MainService

import scala.concurrent._

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    propertiesForServer.flatMap(props =>
      BlazeServerBuilder[IO](ExecutionContext.global)
        .bindHttp(props._1, props._2)
          .withHttpApp(MainService.api)
          .serve
          .compile
          .drain
          .as(ExitCode.Success)
      )
    }
}
