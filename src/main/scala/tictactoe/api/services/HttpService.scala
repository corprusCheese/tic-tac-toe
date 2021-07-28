package tictactoe.api.services

import cats.effect.{Concurrent, ContextShift, IO, Sync, Timer}
import cats.implicits._
import cats.{Monad, MonadThrow}
import core.algebra.AbstractService
import io.circe.Json
import io.circe.syntax.EncoderOps
import org.http4s.HttpRoutes
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.dsl.Http4sDsl
import tictactoe.api.game.{DataHandler, Logic}
import tictactoe.api.game.Game
import core.DataEntities._
import core.structs.{CustomRandom, GameMap}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import tictactoe.api.MainService._
import tictactoe.logs.LogManager

class HttpService[F[_]: Monad: Timer: Concurrent: ContextShift: Sync]
    extends Http4sDsl[F]
    with AbstractService[F] {

  val gameMap: GameMap[F] = new GameMap[F]
  val logManager: LogManager[F] = LogManager[F]()


  private val gameService: HttpRoutes[F] = HttpRoutes.of[F] {

    /** created new game and set it in map */
    case GET -> Root / "board" / "new" =>
      Game[F](3).flatMap { game =>
        val generatedId: gameMap.GameId = CustomRandom.generateGameId(gameMap)
        gameMap.addGame(generatedId, game).pure[F] >>
          game.getJson(generatedId, "Game created").flatMap(json => Ok(json))
      }

    /** get current state of game in map */
    case GET -> Root / "board" / gameId =>
      gameMap.getGame(gameId) match {
        case Some(game) =>
          game.getJson(gameId, "Game is exists").flatMap(json => Ok(json))
        case _ =>
          NotFound("Game is not exist")
      }

    /** clear board in game */
    case GET -> Root / "board" / gameId / "clear" =>
      gameMap
        .clearGame(gameId)
        .flatMap(game => game.getJson(gameId, "Game state cleared").flatMap(json => Ok(json)))

    /** post mark into current board */
    case req @ POST -> Root / "board" / gameId =>
      req
        .as[Json]
        .flatMap { json =>
          MonadThrow[F].fromEither(json.as[Position]).flatMap { position: Position =>
            gameMap.getGame(gameId) match {
              case Some(game) =>
                game.addMark(position).flatMap { x =>
                  x.getJson(gameId, "Mark has been posted").flatMap(json => Ok(json))
                }
              case None =>
                NotFound("Game is not exist")
            }
          }
        }

    /** save game into db */
    case req @ post -> Root / "board" / gameId / "save" =>
      gameMap.getGame(gameId) match {
        case Some(game) =>
          game.getJson.flatMap(json => logManager.insertLog(json) >> Ok("saved!"))
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
