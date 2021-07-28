package core.structs

import cats.Monad
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import core.DataEntities.Position
import tictactoe.api.game.{Game, Logic}

import scala.collection.mutable

class GameMap[F[_]: Sync] {
  private val map: mutable.Map[String, Game[F]] = mutable.Map.empty

  type GameId = String

  def addGame(id: GameId, game: Game[F]): Unit =
    map += (id -> game)

  def removeGame(id: GameId): Unit =
    map -= id

  def updateGame(id: GameId, game: Game[F]): Unit =
    map.update(id, game)

  def getGame(id: GameId): Option[Game[F]] =
    map.get(id)

  def clearGame(id: GameId): F[Game[F]] = {
    Game[F](3).flatMap(newGame => {
      map.update(id, newGame)
      newGame.pure[F]
    })
  }
}
