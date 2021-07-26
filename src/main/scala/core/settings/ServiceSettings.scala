package core.settings

import cats.effect.IO
import cats.implicits.catsSyntaxApplicativeId
import org.http4s.server.middleware.CORSConfig

import scala.concurrent.duration.DurationInt

object ServiceSettings {
  val methodConfig: CORSConfig = CORSConfig(
    anyOrigin = true,
    anyMethod = true,
    allowCredentials = true,
    maxAge = 1.day.toSeconds
  )

  val propertiesForServer: IO[(Int, String)] =  {
    IO(sys.env("PORT").toInt)
      .handleErrorWith(_ => 8080.pure[IO])
      .map(port => (port, "0.0.0.0"))
  }
}
