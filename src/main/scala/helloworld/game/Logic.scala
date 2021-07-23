package helloworld.game

import Logic._
import cats.effect.{Blocker, ContextShift, IO, Timer}
import helloworld.game.Game.count
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.language.{implicitConversions, postfixOps}

object Logic {
  sealed trait Mark
  case object Circle extends Mark
  case object Cross  extends Mark

  sealed trait Result
  case object CircleWins extends Result
  case object CrossWins  extends Result
  case object Draw       extends Result

  val blockingPool: ExecutorService = Executors.newFixedThreadPool(4)
  val blocker: Blocker              = Blocker.liftExecutorService(blockingPool)

  type Board = List[List[Option[Mark]]]

  case class Position(x: Int, y: Int)
  case class Dimension(v: Int)

  implicit def dimension2int(dim: Dimension): Int = dim.v
  implicit def int2dimension(v: Int): Dimension   = Dimension(if (v < 1) 1 else v)

  implicit val encodePosition: Encoder[Position] = (a: Position) =>
    Json.obj(
      ("x", Json.fromInt(a.x)),
      ("y", Json.fromInt(a.y))
    )

  implicit val decodePosition: Decoder[Position] = (c: HCursor) => {
    for {
      x <- c.downField("x").as[Int]
      y <- c.downField("y").as[Int]
    } yield {
      Position(x, y)
    }
  }

  implicit val encodeMark: Encoder[Mark] = (a: Mark) =>
    Json.obj(
      ("mark", DataGameHandler.getResponseFromMark(Option(a)).asJson)
    )
}

class Logic {
  def initBoard(): Board = List(
    List(None, None, None),
    List(None, None, None),
    List(None, None, None)
  )

  def initBoardOfDim(dim: Dimension): Board = {
    var board: Board = List()
    for (_ <- 0 until dim) {
      board = board :+ List.fill(dim)(None)
    }

    board
  }

  def isRowHaveOneMark(list: List[Option[Mark]]): Boolean =
    list.distinct.size == 1 && list.head.isDefined

  def isFull(board: Board): Boolean =
    !board.flatten.contains(None)

  def isSomeoneWin(board: Board): Boolean = {
    getResultFromDiagonals(board) || getResultFromRows(board) || getResultFromRows(board.transpose)
  }

  def getResultFromRows(board: Board): Boolean = {
    (0 until board.size toList).map(x=>isRowHaveOneMark(board(x))).reduce((x, y)=> x || y)
  }

  def getResultFromDiagonals(board: Board): Boolean = {
    val listDiagonal1: List[Option[Mark]] = List.range(0, board.size).map(x=>board(x)(x))
    val listDiagonal2: List[Option[Mark]] = List.range(0, board.size).map(x=>board(board.size - x - 1)(x))

    isRowHaveOneMark(listDiagonal1) || isRowHaveOneMark(listDiagonal2)
  }

  def getResultFromBoard(board: Board, turn: Mark): Option[Result] = {
    val winning: Boolean = isSomeoneWin(board)

    if (winning) {
      turn match {
        case Cross  => Some(CircleWins)
        case Circle => Some(CrossWins)
      }
    } else {
      if (isFull(board)) Some(Draw) else None
    }
  }

  def postMarkToBoard(board: Board, x: Int, y: Int, mark: Mark): Board =
    board(x)(y) match {
      case None    => board.updated(x, board(x).updated(y, Option(mark)))
      case Some(_) => board
    }

  def getNextTurn(turn: Mark): Mark =
    turn match {
      case Circle => Cross
      case Cross  => Circle
    }

  def initState(): (Board, Mark, Option[Result]) =
    (initBoard(), Cross, None)

  def getNextStateAfterPost(board: Board, turn: Mark, position: Position): (Board, Mark) =
    (postMarkToBoard(board, position.x, position.y, turn), getNextTurn(turn))

  def addMarkAndGetNextState(board: Board, turn: Mark, position: Position): (Board, Mark) =
    if (board(position.x)(position.y).isEmpty) {
      val nextState = getNextStateAfterPost(board, turn, position)
      (nextState._1, nextState._2)
    } else {
      (board, turn)
    }

  def saveBoardToElastic(board: Board): Boolean = {
    true
  }
}
