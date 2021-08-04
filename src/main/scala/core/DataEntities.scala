package core

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import tictactoe.api.game.DataHandler

import scala.language.implicitConversions

object DataEntities {
  sealed trait Mark

  case object Circle extends Mark

  case object Cross extends Mark

  sealed trait Result

  case object CircleWins extends Result

  case object CrossWins extends Result

  case object Draw extends Result

  type Board = List[List[Option[Mark]]]

  case class Position(x: Int, y: Int)

  case class Dimension(v: Int)

  implicit def dimension2int(dim: Dimension): Int = dim.v

  implicit def int2dimension(v: Int): Dimension = Dimension(if (v < 1) 1 else v)

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
      ("mark", DataHandler.getResponseFromMark(Option(a)).asJson)
    )
}
