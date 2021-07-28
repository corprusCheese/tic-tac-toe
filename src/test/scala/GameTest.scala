import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import core.DataEntities
import core.DataEntities._
import org.scalatest.FlatSpec
import tictactoe.api.game.Game

class GameTest extends FlatSpec{
  behavior of "functions of Game"

  it should("get turn from game") in {
    val o: IO[DataEntities.Mark] = Game[IO](3).flatMap(game => {
      game.getTurn(turn => {
        turn
      })
    })

    val res: DataEntities.Mark = o.unsafeRunSync()
    assert(res == Cross)
  }

  it should("update turn from game") in {
    val o: IO[DataEntities.Mark] = Game[IO](3).flatMap(game => {
      game.updateTurn()
    })

    val res: DataEntities.Mark = o.unsafeRunSync()
    assert(res == Circle)
  }

  it should("update board from game") in {

    val l = List(
      List(None, Cross.some, None),
      List(None, None, None),
      List(None, None, None)
    )

    val o: IO[Board] = Game[IO](3).flatMap(game => {
      game.updateBoard(l)
    })

    val res = o.unsafeRunSync()
    assert(res == l)
  }


}
