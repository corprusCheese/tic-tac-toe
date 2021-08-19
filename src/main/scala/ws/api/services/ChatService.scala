package ws.api.services

import cats._
import cats.data.Ior
import cats.effect.concurrent._
import cats.effect.{Concurrent, Timer}
import cats.implicits._
import core.algebra.AbstractService
import fs2._
import fs2.concurrent._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import ws.api.chat.WsLogicHandler

import scala.language.implicitConversions

class ChatService[F[_]: Monad: Timer: Concurrent](wsLogicHandler: WsLogicHandler[F])
    extends Http4sDsl[F]
    with AbstractService[F] {
  private val chatService: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "start" =>
      for {
        in  <- wsLogicHandler.inStreamProcessor
        out <- wsLogicHandler.outStream
        res <- WebSocketBuilder[F].build(out, in)
      } yield res
  }

  override def getInstance(): HttpRoutes[F] = chatService
}

object ChatService {

  /** name of person */
  type Name = String

  /** ior of name and stream to client with this name */
  type NameStreamIor[F[_]] = Ior[String, Stream[F, WebSocketFrame]]

  /** also has queue of messages to this client */
  type ComplexIor[F[_]] = Ior[NameStreamIor[F], Queue[F, WebSocketFrame]]

  /** list of persons */
  type IorUserList[F[_]] = List[ComplexIor[F]]

  def apply[F[_]: Concurrent: Timer: Monad](): F[ChatService[F]] =
    for {
      consumers <- Ref.of[F, IorUserList[F]](List.empty[ComplexIor[F]])
    } yield new ChatService(new WsLogicHandler[F](consumers))
}
