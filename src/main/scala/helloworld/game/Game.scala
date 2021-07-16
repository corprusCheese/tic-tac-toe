package helloworld.game

import helloworld.game.Logic.Dimension

object Game {
  val logicService = new Logic
  var (board, turn, result) = logicService.initState()
  val count: Dimension = 3
}
