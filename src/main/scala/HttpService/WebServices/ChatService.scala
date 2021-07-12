package helloworld
package HttpService.WebServices

import helloworld.HttpService.Algebra.AbstractService
import helloworld.HttpService.Game.Logic._

import cats.effect.IO
import cats.effect.concurrent._
import cats.implicits._
import fs2._
import fs2.concurrent._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text

import scala.collection.mutable
import scala.language.implicitConversions


object Streams {
}

class ChatService(queuesList: Ref[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]],
                  messageQueue: Queue[IO, WebSocketFrame],
                  consumersList: Ref[IO, List[Queue[IO, WebSocketFrame]]]) extends Http4sDsl[IO] with AbstractService {

  private val chatService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / "start" =>
          val inStreamProcessor2: Pipe[IO, WebSocketFrame, Unit] = stream => {
            stream.mapAccumulate("")({
              case (name, frame@Text(str, _)) if name == "" =>
                (str, none[String])
              case (name, frame@Text(str, _)) =>
                (name, str.some)
            }).collect({
              case (name, Some(message)) => (name, message)
            }).evalMap({
              case (name, message) =>
                for {
                  consumers <- consumersList.get
                  _ <- consumers.map(queue => {
                    queue.enqueue1(Text(s"$name: $message"))
                  }).sequence
                } yield ()

              //                consumersList.get.flatMap(_.map(_.enqueue1(Text(s"$name: $message"))).sequence).map(_ => ())
            })
          }

          val outStream: Stream[IO, WebSocketFrame] = {
            Stream.eval(Queue.unbounded[IO, WebSocketFrame])
              .evalMap(queue => consumersList.update(queue :: _).map(_ => queue))
              .flatMap(queue => queue.dequeue)
          }

          //            messageQueue.dequeue.evalMap(message => {
          //              queuesList.get.flatMap(queues => {
          //                queues.values.map(_.enqueue1(message)).toList.sequence.map(_ => ())
          //              })
          //            })

          //          val inStreamProcessor: Pipe[IO, WebSocketFrame, Unit] = stream => {
          //            var name = ""
          //
          //            stream.evalMap({
          //              case Text(str, _) if name == "" =>
          //                queuesList.get.flatMap(x => {
          //                  Queue.unbounded[IO, WebSocketFrame].map(q => {
          //                    name = str
          //                    x += (str -> q)
          //
          //                    println(s"$name joined the company!")
          //                  })
          //                })
          //              case Text(str, bool) if name != "" =>
          //                queuesList.update(x => {
          //                  x.get(name) match {
          //                    case Some(y) =>
          //                      messageQueue.enqueue1(Text(s"$name: $str", bool))
          //                      println(messageQueue)
          //                      println(s"$name: $str")
          //                      y.enqueue1(Text(str, bool))
          //                  }
          //
          //                  x
          //                })
          //
          //            })
          //          }

          WebSocketBuilder[IO].build(outStream, inStreamProcessor2)
  }

  override def getInstance(): HttpRoutes[IO] = chatService
}

object ChatService {
  def apply(): IO[ChatService] = for {
    queuesList <- Ref.of[IO, mutable.Map[String, Queue[IO, WebSocketFrame]]](mutable.Map.empty)
    messageQueue <- Queue.unbounded[IO, WebSocketFrame]
    consumers <- Ref.of[IO, List[Queue[IO, WebSocketFrame]]](List.empty)
  } yield new ChatService(queuesList, messageQueue, consumers)
}
