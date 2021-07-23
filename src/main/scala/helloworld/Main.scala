package helloworld

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.server.blaze.BlazeServerBuilder
import cats.implicits._
import helloworld.api.MainService

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  def getPropertiesForServer: IO[(Int, String)] =  {
    IO(sys.env("PORT").toInt)
      .handleErrorWith(_ => 8080.pure[IO])
      .map(port => (port, "0.0.0.0"))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    MainService.api.flatMap(api => {
      getPropertiesForServer.flatMap(props =>
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
