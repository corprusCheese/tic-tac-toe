package tictactoe.api.game

import cats.implicits._
import core.DataEntities._
import io.circe.Json
import tictactoe.api.MainService.logManager
import tictactoe.logs.LogManager

import scala.language.{implicitConversions, postfixOps}

object Logic {
  def initBoard(): Board = List(
    List(None, None, None),
    List(None, None, None),
    List(None, None, None)
  )

  def isRowHaveOneMark(list: List[Option[Mark]]): Boolean =
    list.distinct.size == 1 && list.head.isDefined

  def isFull(board: Board): Boolean =
    !board.flatten.contains(None)

  def isSomeoneWin(board: Board): Boolean =
    getResultFromDiagonals(board) || getResultFromRows(board) || getResultFromRows(board.transpose)

  def getResultFromRows(board: Board): Boolean =
    (board.indices toList).map(x => isRowHaveOneMark(board(x))).reduce((x, y) => x || y)

  def getResultFromDiagonals(board: Board): Boolean = {
    val listDiagonal1: List[Option[Mark]] = List.range(0, board.size).map(x => board(x)(x))
    val listDiagonal2: List[Option[Mark]] =
      List.range(0, board.size).map(x => board(board.size - x - 1)(x))

    isRowHaveOneMark(listDiagonal1) || isRowHaveOneMark(listDiagonal2)
  }

  def getResultFromBoard(game: Game): Option[Result] = {
    val winning: Boolean = isSomeoneWin(game.board)

    if (winning) {
      game.turn match {
        case Cross  => Some(CircleWins)
        case Circle => Some(CrossWins)
      }
    } else {
      if (isFull(game.board)) Some(Draw) else None
    }
  }

  def postMarkToBoard(game: Game, position: Position): Game =
    game.board(position.x)(position.y) match {
      case None =>
        game.board =
          game.board.updated(position.x, game.board(position.x).updated(position.y, game.turn.some))
        game.turn = getNextTurn(game.turn)
        if (game.result.isEmpty)
          game.result = Logic.getResultFromBoard(game)
        game
      case _ => game
    }

  def getNextTurn(turn: Mark): Mark =
    turn match {
      case Circle => Cross
      case Cross  => Circle
    }

  def addMark(game: Game, position: Position): Game =
    if (game.board(position.x)(position.y).isEmpty)
      postMarkToBoard(game, position)
    else game
}
