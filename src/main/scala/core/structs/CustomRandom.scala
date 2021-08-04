package core.structs

import java.util.UUID
import scala.annotation.tailrec

object CustomRandom {
  @tailrec
  final def generateGameId[F[_]](gameMap: GameMap[F]): String = {
    val randomString = UUID.randomUUID().toString
    gameMap.getGame(randomString) match {
      case None => randomString
      case _ => generateGameId(gameMap)
    }
  }
}