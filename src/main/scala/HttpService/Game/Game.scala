package helloworld
package HttpService.Game

import HttpService.Game.Logic.Dimension

object Game {
  val logicService = new Logic
  var (board, turn, result) = logicService.initState()
  val count: Dimension = 3
}
