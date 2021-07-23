package helloworld.api.services

import cats.Monad
import cats.effect.{Blocker, Concurrent, ContextShift, Timer}
import cats.implicits.catsSyntaxApplicativeId
import helloworld.algebra.AbstractService
import helloworld.game.Logic._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Request, Response, StaticFile}

class FileService[F[_]: Monad: Timer: Concurrent: ContextShift] extends Http4sDsl[F] with AbstractService[F] {

  private def static(file: String, blocker: Blocker, request: Request[F]): F[Response[F]] =
    StaticFile.fromResource("/front/public/" + file, blocker, Some(request)).getOrElseF(NotFound())

  private def staticBuild(file: String, blocker: Blocker, request: Request[F]): F[Response[F]] =
    StaticFile
      .fromResource("/front/public/build/" + file, blocker, Some(request))
      .getOrElseF(NotFound())

  private def staticImg(file: String, blocker: Blocker, request: Request[F]): F[Response[F]] =
    StaticFile
      .fromResource("/front/public/img/" + file, blocker, Some(request))
      .getOrElseF(NotFound())

  private val fileService: HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ GET -> Root / path if List(".js", ".css", ".html", ".ico").exists(path.endsWith) =>
      static(path, blocker, req)
    case req @ GET -> Root / "build" / path if List(".js", ".css", ".ico").exists(path.endsWith) =>
      staticBuild(path, blocker, req)
    case req @ GET -> Root / "img" / path if List(".png", ".jpeg", ".jpg").exists(path.endsWith) =>
      staticImg(path, blocker, req)
    case req @ GET -> Root / "global.css" =>
      StaticFile
        .fromResource("/front/public/build/global.css", blocker, Some(req))
        .getOrElseF(NotFound())
    case req @ GET -> Root =>
      StaticFile
        .fromResource(s"/front/public/index.html", blocker, Some(req))
        .getOrElseF(NotFound())
  }

  override def getInstance(): HttpRoutes[F] = fileService
}

object FileService {
  def apply[F[_]: Concurrent: Timer: Monad: ContextShift](): F[FileService[F]] =
    new FileService[F].pure[F]
}
