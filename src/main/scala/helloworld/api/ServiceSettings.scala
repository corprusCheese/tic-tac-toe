package helloworld.api

import org.http4s.server.middleware.CORSConfig

import scala.concurrent.duration.DurationInt

object ServiceSettings {
  val methodConfig: CORSConfig = CORSConfig(
    anyOrigin = true,
    anyMethod = true,
    allowCredentials = true,
    maxAge = 1.day.toSeconds
  )
}
