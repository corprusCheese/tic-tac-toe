package helloworld

import HttpService.MainService

import cats.effect._
import cats.implicits.catsSyntaxApplicativeId
import org.http4s.server.blaze._


object Main extends IOApp {
  def getPropertiesForServer: IO[(Int, String)] =  {
    IO(sys.env("PORT").toInt)
      .handleErrorWith(_ => 8080.pure[IO])
      .map(port => (port, "0.0.0.0"))
  }

  override def run(args: List[String]): IO[ExitCode] = {
    getPropertiesForServer.flatMap(props =>
      BlazeServerBuilder[IO]
        .bindHttp(props._1, props._2)
        .withHttpApp(MainService.api)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    )
  }
}
