package helloworld

import HttpService.HttpService._

import org.scalatest.FlatSpec

class HttpServiceTest extends FlatSpec {
  behavior of "functions of HttpService"

  it should("create a new empty board of size 3x3(initBoard)") in {
    assert(initBoard() == List(
      List(None, None, None),
      List(None, None, None),
      List(None, None, None)
    ))
  }

  it should("create a new empty board of some size(initBoardOfDim)") in {
    assert(initBoardOfDim(3) == initBoard())
    assert(initBoardOfDim(1) == List(List(None)))
    assert(initBoardOfDim(-4) == List(List(None)))
  }

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

    val board1: Board = initBoard()
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

  it should("post mark to board if place is empty and return new Board (postMarkToBoard)") in {
    val ideallyChangedBoard: Board = List(
      List(Some(Circle), None, None),
      List(None, None ,None),
      List(None, None ,None)
    )

    assert(postMarkToBoard(initBoard(), 0, 0, Circle)  == ideallyChangedBoard)
    assert(postMarkToBoard(ideallyChangedBoard, 0, 0, Cross) == ideallyChangedBoard)
  }

  it should("get right result from board (getResultFromBoard)") in {
    val board1 = initBoard()
    val board2 = postMarkToBoard(board1, 0, 0, Cross)
    val board3 = List(
      List(Some(Circle), Some(Circle), Some(Circle)),
      List(None, None, None),
      List(Some(Cross), Some(Cross), Some(Cross))
    )

    assert(getResultFromBoard(board1, Cross).isEmpty)
    assert(getResultFromBoard(board2, Circle).isEmpty)
    assert(getResultFromBoard(board3, Circle).contains(CrossWins))
    assert(getResultFromBoard(board3, Cross).contains(CircleWins))
  }
}
