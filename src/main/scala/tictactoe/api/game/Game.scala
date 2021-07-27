package tictactoe.api.game

import core.DataEntities._
import io.circe.Json
import tictactoe.api.game.Logic.initBoard
import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import io.circe.syntax.EncoderOps

case class Game[F[_]: Sync](dimension: Dimension) {
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
    board.flatMap(_.get.map(callback))

  def getResult[T](callback: Option[Result] => T): F[T] =
    result.flatMap(_.get.map(callback))

  def getTurn[T](callback: Mark => T): F[T] =
    turn.flatMap(_.get.map(callback))

  def updateBoard(newBoard: Board): F[Unit] = {
    println(newBoard)
    board.flatMap(_.update(_ => newBoard))
  }

  def updateTurn(): F[Unit] =
    turn.flatMap(_.update(Logic.getNextTurn))

  def updateResult(): F[Unit] =
    Logic.getResultFromBoard(this).map(r => result.flatMap(_.update(_ => r)))

  def updateAll(board: Board): F[Unit] =
    updateBoard(board).flatMap(_ => updateTurn()).flatMap(_=> getResult(r => if (r.isEmpty) updateResult()))

  val board: F[Ref[F, Board]] = Ref[F].of(initBoard())
  val turn: F[Ref[F, Mark]] = Ref[F].of(Cross)
  val result: F[Ref[F, Option[Result]]] = Ref[F].of(none[Result])
}
