package core.structs

import cats.Monad
import cats.effect.{ContextShift, Sync}
import cats.implicits._
import core.DataEntities.Position
import tictactoe.api.game.{Game, Logic}

import scala.collection.mutable

class GameMap {
  private val map: mutable.Map[String, Game] = mutable.Map.empty

  type GameId = String

  def addGame(id: GameId, game: Game): Unit =
    map += (id -> game)

  def removeGame(id: GameId): Unit =
    map -= id

  def updateGame(id: GameId, game: Game): Unit =
    map.update(id, game)

  def getGame(id: GameId): Option[Game] =
    map.get(id)

  def clearGame(id: GameId): Game = {
    val newGame: Game = Game()
    map.update(id, newGame)
    newGame
  }

  def postNewMark(id: GameId, pos: Position): Option[Game] =
    getGame(id) match {
      case Some(game) =>
        Logic.addMark(game, pos).some
      case None => None
    }
}
