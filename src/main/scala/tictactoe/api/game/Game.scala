package tictactoe.api.game

import core.DataEntities._
import tictactoe.api.game.Logic.initBoard

class Game {
  def initState(): (Board, Mark, Option[Result]) = {
    board = initBoard()
    turn = Cross
    result = None
    (board, turn, result)
  }

  var (board, turn, result) = initState()
  val count: Dimension = 3
}
