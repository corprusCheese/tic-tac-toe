package tictactoe.api.game

import cats.effect.Sync
import cats.implicits._
import core.DataEntities._

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

  def getResultFromGame[F[_]: Sync](game: Game[F]): F[Option[Result]] =
    game
      .getBoard { b =>
        if (isSomeoneWin(b))
          game.getTurn({
            case Cross  => CircleWins.some
            case Circle => CrossWins.some
            case _      => none[Result]
          })
        else {
          val t = if (isFull(b)) Draw.some else none[Result]
          t.pure[F]
        }
      }
      .flatMap(x => x)

  def getNextTurn(turn: Mark): Mark =
    turn match {
      case Circle => Cross
      case Cross  => Circle
    }
}
