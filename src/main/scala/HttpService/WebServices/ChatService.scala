package helloworld
package HttpService.WebServices

import cats.effect.IO
import helloworld.HttpService.Algebra.AbstractService
import helloworld.HttpService.WebServices.FileService.{->, /, GET, Root}
import org.http4s.HttpRoutes
import cats.effect.concurrent._
import fs2.concurrent._
import fs2._
import helloworld.Main.contextShift
import org.http4s.websocket.WebSocketFrame
import org.http4s.Status._

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import cats.implicits._
import helloworld.HttpService.Game.Logic._
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import scala.collection.mutable

object ChatService extends Http4sDsl[IO] with AbstractService {

  private val chatService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / "start" =>
      object ChatStreams {
        def doWork(queuesList: Ref[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]],
                   messageQueue: Queue[IO, WebSocketFrame]): (Stream[IO, WebSocketFrame], Pipe[IO, WebSocketFrame, Unit]) = {
          val outStream: Stream[IO, WebSocketFrame] = messageQueue.dequeue

          val inStreamProcessor: Pipe[IO, WebSocketFrame, Unit] = stream => {
            var name = ""

            stream.evalMap({
              case Text(str, _) if name == "" =>
                queuesList.get.flatMap(x => {
                  Queue.unbounded[IO, WebSocketFrame].map(q => {
                    name = str
                    x += (str -> q)

                    println(s"$name joined the company!")
                  })
                })
              case Text(str, bool) if name != "" =>
                queuesList.get.map(x => {
                  x.get(name) match {
                    case Some(y) =>
                      y.enqueue1(Text(str, bool))
                      messageQueue.enqueue1(Text(s"$name: $str", bool))

                      println(s"$name: $str")
                  }
                })
            })
          }

          (outStream, inStreamProcessor)
        }
      }


      val result = for {
        queuesList <- Ref.of[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]](mutable.Map.empty)
        messageQueue <- Queue.unbounded[IO, WebSocketFrame]
      } yield {
        val str = ChatStreams.doWork(queuesList, messageQueue)
        WebSocketBuilder[IO].build(str._1, str._2)
      }


      result.unsafeRunSync()
  }

  override def getInstance(): HttpRoutes[IO] = chatService
}
