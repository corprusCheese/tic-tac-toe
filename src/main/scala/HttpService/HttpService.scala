package helloworld
package HttpService

import cats.data.Kleisli
import cats.effect._
import io.circe.Json
import io.circe.literal.JsonStringContext
import org.http4s.circe.{jsonDecoder, jsonEncoder}
import org.http4s.dsl.io._
import io.circe._
import io.circe.syntax.EncoderOps
import org.http4s.implicits._
import org.http4s.{HttpRoutes, Request, Response}
import io.circe.{Decoder, Encoder, HCursor, Json}

object HttpService {

  sealed trait Mark
  case object Circle extends Mark
  case object Cross extends Mark

  sealed trait Result
  case object CircleWins extends Result
  case object CrossWins extends Result
  case object Draw extends Result

  type Board = List[List[Option[Mark]]]
  type Dimension = Int

  var turn: Mark = Cross
  var board: Board = initBoard()
  var result: Option[Result] = None
  val count = 3

  case class Position(x: Int, y: Int)

  /*implicit val isPositive: Dimension = Int {
    def apply(): Int = {
      if (this>0) a else 1
    }
  }*/

  implicit val encodeRequest: Encoder[Position] = new Encoder[Position] {
    final def apply(a: Position): Json = Json.obj(
      ("x", Json.fromInt(a.x)),
      ("y", Json.fromInt(a.y))
    )
  }

  implicit val decodeRequest: Decoder[Position] = new Decoder[Position] {
    final def apply(c: HCursor): Decoder.Result[Position] = {
      for {
        x <- c.downField("x").as[Int]
        y <- c.downField("y").as[Int]
      } yield {
        Position(x, y)
      }
    }
  }

  implicit val encodeMark: Encoder[Mark] = new Encoder[Mark] {
    final def apply(a: Mark): Json = Json.obj(
      ("mark", getResponseFromMark(Option(a)).asJson)
    )
  }

  def getMarkFromRequest(str: String): Mark = {
    str match {
      case "x" => Cross
      case "o" => Circle
    }
  }

  def getResponseFromMark(mark: Option[Mark]): String = {
    mark match {
      case Some(Circle) => "x"
      case Some(Cross) => "o"
      case _ => "none"
    }
  }

  def getNextTurn(turn: Mark): Mark = {
    turn match {
      case Circle => Cross
      case Cross => Circle
    }
  }

  def getResultToString(result: Option[Result]): String = {
    result match {
      case Some(Draw) => "draw"
      case Some(CircleWins) => "circle wins"
      case Some(CrossWins) => "cross wins"
      case _ => "none"
    }
  }

  def isRowHaveOneMark(list: List[Option[Mark]]): Boolean =
    list.distinct.size == 1 && list.head.isDefined

  def isSomeoneWin(board: Board): Boolean = {
    val boardT: Board = board.transpose
    var result = false;

    var listDiagonal1: List[Option[Mark]] = List()
    var listDiagonal2: List[Option[Mark]] = List()
    for (i <- 0 until count) {
      result = result || isRowHaveOneMark(board(i)) || isRowHaveOneMark(boardT(i))

      listDiagonal1 = listDiagonal1 :+ board(i)(i)
      listDiagonal2 = listDiagonal2 :+ board(count-1-i)(i)
    }

    result = result || isRowHaveOneMark(listDiagonal1) || isRowHaveOneMark(listDiagonal2)

    result
  }

  def initBoard() : Board = List(
    List(None, None, None),
    List(None, None, None),
    List(None, None, None)
  )

  def initBoardOfDim(dim: Int): Board = {



  }

  def getBoardToString(board: Board): String = {
    board.map(x => x.map(y => getResponseFromMark(y))).transpose.toString()
  }

  def postMarkToBoard(board: Board, x: Int, y: Int, mark: Mark): Board = {
    board(x)(y) match {
      case None => board.updated(x, board(x).updated(y, Option(mark)))
      case Some(value) => board
    }
  }

  def isFull(board: Board): Boolean =
    !board.flatten.contains(None)

  def getResultFromBoard(board: Board, turn: Mark): Option[Result] = {
    val winning: Boolean = isSomeoneWin(board)

    if (winning) {
      turn match {
        case Cross => Some(CircleWins)
        case Circle => Some(CrossWins)
      }
    } else {
      if (isFull(board)) Some(Draw) else None
    }
  }

  val gameService: Kleisli[IO, Request[IO], Response[IO]] = HttpRoutes.of[IO] {
    case GET -> Root / "board" =>
      if (result.isEmpty) {
        result = getResultFromBoard(board, turn)
      }

      val stringBoard: String = getBoardToString(board)
      val stringTurn: String = getResponseFromMark(Option(turn))
      val stringResult: String = getResultToString(result)

      val json = json"""{"turn": $stringTurn, "result": $stringResult, "field": $stringBoard}"""

      Ok(json)
    case req@POST -> Root / "board" =>
      req.as[Json].flatMap(json => {
        val maybePosition: Either[DecodingFailure, Position] = for {ans <- json.as[Position]} yield ans

        maybePosition match {
          case Right(position: Position) =>
            if (board(position.x)(position.y).isEmpty) {
              board = postMarkToBoard(board, position.x, position.y, turn)
              turn = getNextTurn(turn)
            }

            val stringBoard: String = getBoardToString(board)
            val jsonText = json"""{"field": $stringBoard}"""

            Ok(jsonText)
          case Left(x) =>
            Ok(x.toString())
        }
      })

  }.orNotFound
}
