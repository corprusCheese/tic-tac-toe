package tictactoe.api.game

import cats.effect.Sync
import cats.implicits._
import core.DataEntities._

import java.util.concurrent.atomic.AtomicInteger
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

  def getResultFromBoard[F[_]: Sync](game: Game[F]): F[Option[Result]] =
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

  def setPositionToBoard(board: Board, position: Position, mark: Mark): Board = {
    val incRow = new AtomicInteger(0)
    board.map { row =>
      val incCell = new AtomicInteger(0)
      val newRow = row.map { cell =>
        val newCell =
          if (position.x == incCell.get() && position.y == incRow.get()) mark.some else cell
        incCell.incrementAndGet()
        newCell
      }
      println(incRow.incrementAndGet())
      newRow
    }

  }

  def postMarkToBoard[F[_]: Sync](game: Game[F], position: Position): F[Game[F]] = {
    println("post method")
    game
      .getBoard(b =>
        b(position.x)(position.y) match {
          case None =>
            println("set value")
            game.updateBoard(setPositionToBoard(b, position, Cross))
        }
      )
      .map(_ => game)
  }

  def getNextTurn(turn: Mark): Mark =
    turn match {
      case Circle => Cross
      case Cross  => Circle
    }

  def addMark[F[_]: Sync](game: Game[F], position: Position): F[Game[F]] =
    game
      .getBoard { b =>
        println("game")
        if (b(position.x)(position.y).isEmpty)
          postMarkToBoard(game, position)
        else
          game.pure[F]
      }
      .flatMap(x => x)
}
