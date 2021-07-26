package tictactoe.api.game

import core.DataEntities._
import io.circe.Json
import tictactoe.api.game.Logic.initBoard
import cats._
import cats.implicits._
import io.circe.syntax.EncoderOps

case class Game() {
  def getJson: Json = {
    Json.obj(
      "board" -> board.asJson,
      "result" -> DataHandler.getResultToString(result).asJson
    )
  }

  def initState(): (Board, Mark, Option[Result]) = {
    board = initBoard()
    turn = Cross
    result = None
    (board, turn, result)
  }

  // TODO: var to val REF
  var (board, turn, result) = initState()
  val count: Dimension = 3
}
