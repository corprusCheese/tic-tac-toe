package tictactoe.api.game

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import core.DataEntities._
import io.circe.Json
import io.circe.syntax.EncoderOps
import tictactoe.api.game.Logic._

case class Game[F[_]: Sync](
  board: Ref[F, Board],
  turn: Ref[F, Mark],
  result: Ref[F, Option[Result]],
  dimension: Dimension
) {
  def getJson(id: String, message: String): F[Json] =
    for {
      b <- getBoard(b => DataHandler.getBoardAsMap(b).asJson)
      r <- getResult(r => DataHandler.getResultToString(r).asJson)
      t <- getTurn(t => DataHandler.getResponseFromMark(t.some).asJson)
    } yield Json.obj(
      "board"   -> b,
      "result"  -> r,
      "turn"    -> t,
      "id"      -> id.asJson,
      "message" -> message.asJson
    )

  def getJson: F[Json] =
    for {
      b <- getBoard(b => DataHandler.getBoardAsMap(b).asJson)
      r <- getResult(r => DataHandler.getResultToString(r).asJson)
      t <- getTurn(t => DataHandler.getResponseFromMark(t.some).asJson)
    } yield Json.obj(
      "board"  -> b,
      "result" -> r,
      "turn"   -> t
    )

  def getBoard[T](callback: Board => T): F[T] =
    board.get.map(callback)

  def getResult[T](callback: Option[Result] => T): F[T] =
    result.get.map(callback)

  def getTurn[T](callback: Mark => T): F[T] =
    turn.get.map(callback)

  def updateBoard(newBoard: Board): F[Board] =
    board.updateAndGet(x => newBoard)

  def updateTurn(): F[Mark] =
    turn.updateAndGet(Logic.getNextTurn)

  def updateResult(): F[Option[Result]] =
    Logic.getResultFromGame(this).flatMap(res => result.updateAndGet(_ => res))

  def postMarkToBoard(position: Position): F[Game[F]] =
    getBoard(b =>
      b(position.x)(position.y) match {
        case None =>
          turn.get.flatMap(t =>
            updateBoard(Game.setPositionToBoard(b, position, t)) >>
              updateTurn() >>
              updateResult()
          )
      }
    ).flatMap(x => x).flatMap { _ =>
      this.asInstanceOf[Game[F]].pure[F]
    }

  def addMark(position: Position): F[Game[F]] =
    getBoard { b =>
      if (b(position.x)(position.y).isEmpty)
        postMarkToBoard(position)
      else
        this.asInstanceOf[Game[F]].pure[F]
    }.flatMap(x => x)
}

object Game {
  def initBoard(): Board = List(
    List(None, None, None),
    List(None, None, None),
    List(None, None, None)
  )

  def setPositionToBoard(board: Board, position: Position, mark: Mark): Board =
    board.iterator.zipWithIndex.map {
      case (row, i) =>
        row.iterator.zipWithIndex.map {
          case (cell, j) => if (i == position.x && j == position.y) mark.some else cell
        }.toList
    }.toList

  def apply[F[_]: Sync](dimension: Dimension): F[Game[F]] =
    (Ref[F].of[Board](initBoard()), Ref[F].of[Mark](Cross), Ref[F].of[Option[Result]](none[Result]))
      .mapN((board, turn, result) => new Game(board, turn, result, dimension))
}
