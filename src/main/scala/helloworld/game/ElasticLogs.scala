package helloworld.game

import com.sksamuel.elastic4s.ElasticApi._
import com.sksamuel.elastic4s.http._
import com.sksamuel.elastic4s._
import com.sksamuel.elastic4s.fields._
import com.sksamuel.elastic4s.http.JavaClient
import com.sksamuel.elastic4s.requests.common._
import com.sksamuel.elastic4s.requests.searches._
import helloworld.game.Logic._

class ElasticLogs {
  val props: ElasticProperties = ElasticProperties("http://localhost:9200")
  val client: ElasticClient = ElasticClient(JavaClient(props))

  import com.sksamuel.elastic4s.ElasticDsl._

  client.execute {
    createIndex("resultBoards").mapping(
      properties(
        TextField("result"),
        TextField("board")
      )
    )
  }.await

  def putIntoLogs(result: Result, board: Board): Unit = {
    client.execute {
      indexInto("resultBoards").fields("result" -> result.toString, "board" -> board.toString()).refresh(RefreshPolicy.Immediate)
    }.await
  }

  def findBoardsCountByResultInLogs(result: Result): Unit = {
    val resp = client.execute {
      search("resultBoards").query(result.toString)
    }.await

    resp foreach (search => println(s"There were ${search.totalHits} total hits"))
  }
}


object ElasticLogs {
  def apply: ElasticLogs = new ElasticLogs
}