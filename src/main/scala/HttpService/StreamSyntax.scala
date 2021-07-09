package helloworld
package HttpService

import fs2.{Pipe, Stream}
import fs2.concurrent.Queue
import cats.implicits._

trait StreamSyntax {

  implicit class StreamSyntax[F[_], A](val f: Stream[F, A]) {
    def fromQueueNoneTerminated(q: Queue[F, Option[A]]): Stream[F, A] =
      q.dequeue.unNoneTerminate

    def enqueueNoneTerminated(q: Queue[F, Option[A]]): Pipe[F, A, Unit] = s =>
      s.map(_.some).through(q.enqueue)
  }
}
