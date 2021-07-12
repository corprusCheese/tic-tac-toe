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


object Streams {
  val streams: IO[(Ref[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]], Queue[IO, WebSocketFrame])] = for {
    queuesList <- Ref.of[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]](mutable.Map.empty)
    messageQueue <- Queue.unbounded[IO, WebSocketFrame]
  } yield (queuesList, messageQueue)
}

object ChatService extends Http4sDsl[IO] with AbstractService {

  private val chatService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / "start" =>
      object ChatStreams {
        def doWork(queuesList: Ref[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]],
                   messageQueue: Queue[IO, WebSocketFrame]): (Stream[IO, WebSocketFrame], Pipe[IO, WebSocketFrame, Unit]) = {
          val outStream: Stream[IO, WebSocketFrame] = {
            messageQueue.dequeue
          }

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
                queuesList.update(x => {
                  x.get(name) match {
                    case Some(y) =>
                      messageQueue.enqueue1(Text(s"$name: $str", bool))
                      println(messageQueue)
                      println(s"$name: $str")
                      y.enqueue1(Text(str, bool))
                  }

                  x
                })

            })
          }

          (outStream, inStreamProcessor)
        }
      }


      val result = for {
        streams <- Streams.streams
      } yield {
        val str = ChatStreams.doWork(streams._1, streams._2)
        WebSocketBuilder[IO].build(str._1, str._2)
      }


      result.unsafeRunSync()
  }

  override def getInstance(): HttpRoutes[IO] = chatService
}
