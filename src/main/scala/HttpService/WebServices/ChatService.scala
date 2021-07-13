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
import cats._
import cats.data.Ior
import cats.effect.{Concurrent, Timer}
import cats.implicits._

import scala.collection.mutable
import scala.language.implicitConversions
import cats.data.Ior
import helloworld.HttpService.WebServices.ChatService.IorUserList


class ChatService(consumersListOfIors: Ref[IO, IorUserList]) extends Http4sDsl[IO] with AbstractService {

  private val chatService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case req@GET -> Root / "start" =>
      // second
      val inStreamProcessor2: Pipe[IO, WebSocketFrame, Unit] = stream => {

        println(3)
        /*consumersListOfIors.get.map(iorUserList => {
          iorUserList.map(ior => {
            val newIor = ior.left match {
              case None =>
                ior.putLeft(stream)
                ior
              case _ => ior
            }
            newIor
          })
          println(iorUserList)
          iorUserList
        })*/
        val ior: Ior.Left[Stream[IO, WebSocketFrame]] = Ior.Left(stream)

        consumersListOfIors.update(ior::_)

        consumersListOfIors.get.map(iorUserList => {
          iorUserList.map(ior => {
            println(ior)
            ior
          })
          iorUserList
        })

        println(ior)
        println(consumersListOfIors.get)

        println(4)

        stream.mapAccumulate("")({
          case (name, frame@Text(str, _)) if name == "" =>
            (str, none[String])
          case (name, frame@Text(str, _)) =>
            (name, str.some)
        }).collect({
          case (name, Some(message)) => (name, message)
        }).evalMap({
          case (name, message) =>
            consumersListOfIors
              .get
              .flatMap(_.map(ior => {
                println(ior.left.get)
                println(stream)
                println(ior.left.get != stream)
                if (ior.left.get != stream) {
                  ior.right.get.enqueue1(Text(s"$name: $message"))
                } else {
                  ior.right.get.enqueue1(Text(s"You send the message!"))
                }
              }).sequence).map(_ => ())
        })
      }

      // 1 3 4 5 2
      val outStream: Stream[IO, WebSocketFrame] = {
        println(1)
        Stream.eval(Queue.unbounded[IO, WebSocketFrame])
          .evalMap(queue => {
            println(5)
            consumersListOfIors.update(iorUserList => {
              iorUserList.map(ior => {
                println(ior)
                val newIor = ior.right match {
                  case None =>
                    ior.putRight(queue)
                  case _ => ior
                }
                newIor
              })
            }).map(_=>queue)
            //consumersListOfIors.update(ior::_).map(_ => queue)
          }).flatMap(queue => {
          println(2)
          queue.dequeue
        })
      }
      WebSocketBuilder[IO].build(outStream, inStreamProcessor2)
  }

  override def getInstance(): HttpRoutes[IO] = chatService
}

object ChatService {

  type IorUserList = List[Ior[Stream[IO, WebSocketFrame], Queue[IO, WebSocketFrame]]]

  def apply(): IO[ChatService] = for {
    consumers <- Ref.of[IO, IorUserList](List.empty)
  } yield new ChatService(consumers)
}
