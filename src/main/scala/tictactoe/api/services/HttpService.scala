package tictactoe.api.services

import cats.effect.{Concurrent, ContextShift, Timer}
import cats.implicits._
import cats.{Monad, MonadThrow}
import core.algebra.AbstractService
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s.HttpRoutes
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.dsl.Http4sDsl
import tictactoe.api.DataBootstrapper.{CustomRandom, GameMap, gameMap}
import tictactoe.api.game.{DataHandler, Logic}
import tictactoe.api.game.Game
import core.DataEntities._
import tictactoe.api.MainService.logManager

class HttpService [F[_]: Monad: Timer: Concurrent: ContextShift] extends Http4sDsl[F] with AbstractService[F] {

  private def getJsonAsResponse(game: Game, id: String, message: Option[String]): Json = {
    message match {
      case None =>
        Json.obj(
          "turn"   -> DataHandler.getResponseFromMark(Option(game.turn)).asJson,
          "result" -> DataHandler.getResultToString(game.result).asJson,
          "board"  -> DataHandler.getBoardAsMap(game.board).asJson,
          "id" -> id.asJson
        )
      case Some(value) =>
        Json.obj(
          "turn"   -> DataHandler.getResponseFromMark(Option(game.turn)).asJson,
          "result" -> DataHandler.getResultToString(game.result).asJson,
          "board"  -> DataHandler.getBoardAsMap(game.board).asJson,
          "id" -> id.asJson,
          "message" -> value.asJson
      )
    }
  }

  private val gameService: HttpRoutes[F] = HttpRoutes.of[F] {
    /** created new game and set it in map */
    case GET -> Root / "board" / "new" =>
      val game: Game = new Game()
      val generatedId: gameMap.GameId = CustomRandom.generateGameId(gameMap)
      gameMap.addGame(generatedId, game)
      Ok(getJsonAsResponse(game, generatedId, "Game created".some))
    /** get current state of game in map */
    case GET -> Root / "board" / gameId =>
      gameMap.getGame(gameId) match {
        case Some(game) =>
          Ok(
            getJsonAsResponse(
              game,
              gameId,
              "Game is exist".some
            )
          )
        case _ =>
          NotFound("Game is not exist")
      }
    /** clear board in game */
    case GET -> Root / "board" / gameId / "clear" =>
      val game: Game = gameMap.clearGame(gameId)
      Ok(getJsonAsResponse(game, gameId, "Game state cleared".some))
    /** post mark into current board */
    case req @ POST -> Root / "board" / gameId =>
       req.as[Json]
         .flatMap { json =>
           MonadThrow[F].fromEither(json.as[Position]).flatMap { position: Position =>
             gameMap.postNewMark(gameId, position) match {
               case Some(game) =>
                 gameMap.updateGame(gameId, game)
                 Ok(getJsonAsResponse(game, gameId, "Mark has been posted".some))
               case None =>
                 NotFound("Game is not exist")
             }
           }
         }
    /** save game into db */
    case req @ post -> Root / "board"/ gameId / "save" =>
      gameMap.getGame(gameId) match {
        case Some(game) =>
          logManager.insertLog(game.getJson)
          Ok("saved!")
        case _ => NotFound("error")
      }
  }

  override def getInstance(): HttpRoutes[F] = gameService
}

object HttpService {
  def apply[F[_]: Concurrent: Timer: Monad: ContextShift](): F[HttpService[F]] =
    new HttpService[F].pure[F]

  def create[F[_]: Concurrent: Timer: Monad: ContextShift](): HttpService[F] =
    new HttpService[F]
}
