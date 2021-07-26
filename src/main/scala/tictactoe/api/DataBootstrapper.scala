package tictactoe.api

import cats.implicits.catsSyntaxOptionId
import tictactoe.api.DataBootstrapper.gameMap.GameId
import core.DataEntities.Position

import scala.collection.mutable
import tictactoe.api.game.{Game, Logic}

import java.util.UUID
import scala.annotation.tailrec
import scala.util.Random

object DataBootstrapper {
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

  object CustomRandom {
    @tailrec
    final def generateGameId(gameMap: GameMap): GameId = {
      val randomString = UUID.randomUUID().toString
      gameMap.getGame(randomString) match {
        case None => randomString
        case _ => generateGameId(gameMap)
      }
    }
  }

  val gameMap: GameMap = new GameMap
}
