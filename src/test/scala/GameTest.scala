import cats.effect.IO
import cats.implicits.{catsSyntaxFlatMapOps, catsSyntaxOptionId, none}
import core.DataEntities
import core.DataEntities._
import org.scalatest.FlatSpec
import tictactoe.api.game.Game
import tictactoe.api.game.Game._

class GameTest extends FlatSpec {
  behavior.of("functions of Game")

  it should ("get turn from game") in {
    val o: IO[DataEntities.Mark] = Game[IO](3).flatMap { game =>
      game.getTurn { turn =>
        turn
      }
    }

    val res: DataEntities.Mark = o.unsafeRunSync()
    assert(res == Cross)
  }

  it should ("get board from game") in {
    val o: IO[Board] = Game[IO](3).flatMap { game =>
      game.getBoard(board => board)
    }

    val res = o.unsafeRunSync()
    assert(res == initBoard())
  }

  it should ("get result from game") in {
    val o = Game[IO](3).flatMap { game =>
      game.getResult(result => result)
    }

    val res = o.unsafeRunSync()
    assert(res == none[Result])
  }

  it should ("update result from game") in {
    val winningCase = List(
      List(Cross.some, Cross.some, Cross.some),
      List(Circle.some, Circle.some, None),
      List(None, None, None)
    )

    val o = Game[IO](3).flatMap { game =>
      game.updateBoard(winningCase) >>
        // equivalent cross circle x3
        game.updateTurn() >>
        game.updateResult()
    }

    val res = o.unsafeRunSync()
    assert(res == CrossWins.some)
  }

  it should ("update turn from game") in {
    val o: IO[DataEntities.Mark] = Game[IO](3).flatMap { game =>
      game.updateTurn()
    }

    val res: DataEntities.Mark = o.unsafeRunSync()
    assert(res == Circle)
  }

  val firstMoveBoard = List(
    List(Cross.some, None , None),
    List(None, None, None),
    List(None, None, None)
  )

  it should ("update board from game") in {
    val o: IO[Board] = Game[IO](3).flatMap { game =>
      game.updateBoard(firstMoveBoard)
    }

    val res = o.unsafeRunSync()
    assert(res == firstMoveBoard)
  }

  it should ("post mark to board") in {
    val o = Game[IO](3).flatMap { game =>
      game.postMarkToBoard(Position(0, 0)) >>
        game.getBoard(board => board)
    }

    val res = o.unsafeRunSync()
    assert(res == firstMoveBoard)
  }

  it should ("return new board from immutable Board") in {
    assert(Game.setPositionToBoard(initBoard(), Position(0, 0), Cross) == firstMoveBoard)
  }

  it should ("return same game with same board if position is not empty") in {
    val o = Game[IO](3).flatMap { game =>
      game.addMark(Position(0, 0)) >>
        game.updateTurn() >>
        game.addMark(Position(0, 0)) >>
        game.getBoard(board => board)
    }

    val res = o.unsafeRunSync()
    assert(res == firstMoveBoard)
  }
}
