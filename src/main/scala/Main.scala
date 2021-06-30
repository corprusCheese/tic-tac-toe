package helloworld

import HttpService.HttpService

import cats.effect._
import org.http4s.server.blaze._

import scala.util.{Failure, Success, Try}


object Main extends IOApp {

  def getPropertiesForServer: (Int, String) =  {
    Try(sys.env("PORT").toInt) match {
      case Success(p) => (p, "0.0.0.0")
      case Failure(_) => (8080, "localhost")
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val propertiesForServer = getPropertiesForServer
    BlazeServerBuilder[IO]
      .bindHttp(propertiesForServer._1, propertiesForServer._2)
      .withHttpApp(HttpService.gameService)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
