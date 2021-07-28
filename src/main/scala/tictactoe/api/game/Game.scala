package tictactoe.api.game

import core.DataEntities._
import io.circe.Json
import tictactoe.api.game.Logic.initBoard
import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import io.circe.syntax.EncoderOps

case class Game[F[_]: Sync](board: Ref[F, Board], turn: Ref[F, Mark], result:Ref[F, Option[Result]], dimension: Dimension) {
  def getJson(id: String, message: String): F[Json] =
    for {
      b <- getBoard(b => DataHandler.getBoardAsMap(b).asJson)
      r <- getResult(r => DataHandler.getResultToString(r).asJson)
      t <- getTurn(t => DataHandler.getResponseFromMark(t.some).asJson)
    } yield Json.obj(
      "board"  -> b,
      "result" -> r,
      "turn" -> t,
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
      "turn" -> t
    )

  def getBoard[T](callback: Board => T): F[T] =
    board.get.map(callback)

  def getResult[T](callback: Option[Result] => T): F[T] =
    result.get.map(callback)

  def getTurn[T](callback: Mark => T): F[T] =
    turn.get.map(callback)

  def updateBoard(newBoard: Board): F[Unit] =
    board.set(newBoard)

  def updateTurn(): F[Unit] =
    turn.update(Logic.getNextTurn)

  def updateResult(): F[Unit] =
    Logic.getResultFromBoard(this).map(r => result.set(r))
}

object Game {
  def apply[F[_]: Sync](dimension: Dimension): F[Game[F]] = {
    (Ref[F].of[Board](initBoard()), Ref[F].of[Mark](Cross), Ref[F].of[Option[Result]](none[Result]))
      .mapN((board, turn, result) => new Game(board, turn, result, dimension))
  }
}
