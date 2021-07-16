package helloworld.HttpService.Game

import Logic.Dimension

object Game {
  val logicService = new Logic
  var (board, turn, result) = logicService.initState()
  val count: Dimension = 3
}
