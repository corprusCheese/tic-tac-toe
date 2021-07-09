package helloworld
package HttpService.Game

import HttpService.Game.Logic.{Board, Circle, CircleWins, Cross, CrossWins, Draw, Mark, Result}

object DataGameHandler {
  def getBoardAsMap(board: Board): Map[Int, Map[Int, String]] = {
    board.map(x => x.map(y => getResponseFromMark(y))).transpose
      .map(x=>x.zipWithIndex.map{ case (v,i) => (i,v) }.toMap).zipWithIndex.map{ case (v,i) => (i,v) }.toMap
  }
  def getMarkFromRequest(str: String): Mark = {
    str match {
      case "x" => Cross
      case "o" => Circle
    }
  }

  def getResponseFromMark(mark: Option[Mark]): String = {
    mark match {
      case Some(Circle) => "o"
      case Some(Cross) => "x"
      case _ => "none"
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
}
