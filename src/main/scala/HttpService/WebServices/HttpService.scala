package helloworld
package HttpService.WebServices

import cats.data.Kleisli
import cats.effect.IO
import helloworld.HttpService.Game.DataGameHandler
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.{Callback, HttpRoutes, Request, Response}
import helloworld.HttpService.Game.Game._
import helloworld.HttpService.Game.Logic.{cs, _}
import io.circe.syntax.EncoderOps
import org.http4s.circe.{jsonDecoder, jsonEncoder}

import helloworld.HttpService.Algebra.AbstractService


object HttpService extends Http4sDsl[IO] with AbstractService {

  private def getJsonBoardOnlyAsResponse(board: Board): Json = {
    Json.obj("board" -> DataGameHandler.getBoardAsMap(board).asJson)
  }

  private def getJsonAsResponse(board: Board, turn: Mark, result: Option[Result]): Json = {
    Json.obj(
      "turn" -> DataGameHandler.getResponseFromMark(Option(turn)).asJson,
      "result" -> DataGameHandler.getResultToString(result).asJson,
      "board" -> DataGameHandler.getBoardAsMap(board).asJson
    )
  }

  private val gameService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "board" =>
      Ok(getJsonAsResponse(
        board,
        turn,
        if (result.isEmpty)
          logicService.getResultFromBoard(board, turn)
        else result
      ))
    case GET -> Root / "board" / "clear" =>
      val items = logicService.initState()
      board = items._1
      turn = items._2
      result = items._3

      Ok(getJsonAsResponse(
        board,
        turn,
        result
      ))
    case req@POST -> Root / "board" =>
      req.as[Json].flatMap(json => {
        IO.fromEither(json.as[Position]).flatMap {
          position: Position =>
            val state = logicService.addMarkAndGetNextState(
              board,
              turn,
              position
            )

            board = state._1
            turn = state._2

            Ok(getJsonBoardOnlyAsResponse(board))
        }
      })
  }

  override def getInstance(): HttpRoutes[IO] = gameService
}
