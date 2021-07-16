package helloworld.HttpService.WebServices

import cats.effect.{Blocker, IO}
import helloworld.game.Logic.{cs, _}
import helloworld.algebra.AbstractService
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Request, Response, StaticFile}

// todo: rewrite with F instead of IO
object FileService extends Http4sDsl[IO] with AbstractService[IO] {

  private def static(file: String, blocker: Blocker, request: Request[IO]): IO[Response[IO]] =
    StaticFile.fromResource("/front/public/" + file, blocker, Some(request)).getOrElseF(NotFound())

  private def staticBuild(file: String, blocker: Blocker, request: Request[IO]): IO[Response[IO]] =
    StaticFile
      .fromResource("/front/public/build/" + file, blocker, Some(request))
      .getOrElseF(NotFound())

  private val fileService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req @ GET -> Root / path if List(".js", ".css", ".html", ".ico").exists(path.endsWith) =>
      static(path, blocker, req)
    case req @ GET -> Root / "build" / path if List(".js", ".css", ".ico").exists(path.endsWith) =>
      staticBuild(path, blocker, req)
    case req @ GET -> Root / "global.css" =>
      StaticFile
        .fromResource("/front/public/build/global.css", blocker, Some(req))
        .getOrElseF(NotFound())
    case req @ GET -> Root =>
      StaticFile
        .fromResource(s"/front/public/index.html", blocker, Some(req))
        .getOrElseF(NotFound())
  }

  override def getInstance(): HttpRoutes[IO] = fileService
}
