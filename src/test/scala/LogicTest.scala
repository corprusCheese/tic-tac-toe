import core.DataEntities._
import org.scalatest.FlatSpec
import tictactoe.api.game.Game
import tictactoe.api.game.Logic._

class LogicTest extends FlatSpec {
  behavior of "functions of Logic"

  it should("notice winning line if it has only elems of one type but not None(isRowHaveOneMark)") in {
    val list1: List[Option[Mark]] = List(None, None, None)
    val list2: List[Option[Mark]] = List(None, Some(Circle), None)
    val list3: List[Option[Mark]] = List(Some(Circle), Some(Circle), Some(Circle))

    assert(!isRowHaveOneMark(list1))
    assert(!isRowHaveOneMark(list2))
    assert(isRowHaveOneMark(list3))
  }

  it should("notice winning case when board has diagonal or vertical or horizontal lines " +
    "that input to the previous function (isSomeoneWin)") in {

    val board1: Board = Game.initBoard()
    val board2: Board = List(
      List(None, None, None),
      List(None, Some(Circle), None),
      List(Some(Circle), Some(Circle), Some(Circle))
    )
    val board3: Board = List(
      List(Some(Circle), None, None),
      List(None, Some(Circle), None),
      List(None, Some(Circle), Some(Circle))
    )

    assert(!isSomeoneWin(board1))
    assert(isSomeoneWin(board2))
    assert(isSomeoneWin(board3))
  }

  // curl --request POST http://127.0.0.1:8080/board --data '{"x": 0, "y": 1}'
}
