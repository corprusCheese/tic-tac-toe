package ws

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze.BlazeServerBuilder
import cats.implicits._
import core.settings.ServiceSettings.propertiesForServer
import ws.api._

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    MainService.api.flatMap(api => {
      propertiesForServer.flatMap(props =>
        BlazeServerBuilder[IO](ExecutionContext.global)
          .bindHttp(props._1, props._2)
          .withHttpApp(api)
          .serve
          .compile
          .drain
          .as(ExitCode.Success)
      )
    })
  }
}
