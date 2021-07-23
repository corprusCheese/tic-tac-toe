package helloworld.api.services

import com.sksamuel.elastic4s.ElasticDsl._
import cats.{Monad, MonadThrow}
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import helloworld.game.Game._
import helloworld.algebra.AbstractService
import helloworld.game.DataGameHandler
import helloworld.game.Logic._
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s.HttpRoutes
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.dsl.Http4sDsl
import io.circe.generic.auto._

class HttpService [F[_]: Monad: Timer: Concurrent: ContextShift] extends Http4sDsl[F] with AbstractService[F] {

  private def getJsonBoardOnlyAsResponse(board: Board): Json =
    Json.obj("board" -> DataGameHandler.getBoardAsMap(board).asJson)

  private def getJsonAsResponse(board: Board, turn: Mark, result: Option[Result]): Json =
    Json.obj(
      "turn"   -> DataGameHandler.getResponseFromMark(Option(turn)).asJson,
      "result" -> DataGameHandler.getResultToString(result).asJson,
      "board"  -> DataGameHandler.getBoardAsMap(board).asJson
    )

  private val gameService: HttpRoutes[F] = HttpRoutes.of[F] {
    case GET -> Root / "board" =>
      Ok(
        getJsonAsResponse(
          board,
          turn,
          if (result.isEmpty)
            logicService.getResultFromBoard(board, turn)
          else result
        )
      )
    case GET -> Root / "board" / "clear" =>
      val items = logicService.initState()
      board = items._1
      turn = items._2
      result = items._3

      Ok(
        getJsonAsResponse(
          board,
          turn,
          result
        )
      )
    case req @ POST -> Root / "board" =>
       req.as[Json]
         .flatMap { json =>
           MonadThrow[F].fromEither(json.as[Position]).flatMap { position: Position =>
             val state = logicService.addMarkAndGetNextState(board, turn, position)
             board = state._1
             turn = state._2
             //Ok(getJsonBoardOnlyAsResponse(board))
             if (result.isEmpty)
               result = logicService.getResultFromBoard(board, turn)
             Ok(
               getJsonAsResponse(
                 board,
                 turn,
                 result
               )
             )
           }
         }
    case req @ post -> Root / "board"/ "save" =>
      logicService.saveBoardToElastic(board)
      Ok("saved!")
  }

  override def getInstance(): HttpRoutes[F] = gameService
}

object HttpService {
  def apply[F[_]: Concurrent: Timer: Monad: ContextShift](): F[HttpService[F]] =
    new HttpService[F].pure[F]

  def create[F[_]: Concurrent: Timer: Monad: ContextShift](): HttpService[F] =
    new HttpService[F]
}
