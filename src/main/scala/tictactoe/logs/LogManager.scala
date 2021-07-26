package tictactoe.logs

import cats._
import cats.Monad
import cats.effect._
import doobie.{Transactor, _}
import doobie._
import doobie.implicits._
import doobie.util.transactor.Transactor.Aux
import io.circe.Json

class LogManager [F[_]: Monad] {
  private implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContexts.synchronous)
  val xa: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",     // driver classname
    "jdbc:postgresql:logs",     // connect URL (driver-specific)
    "postgres",                  // user
    "",                          // password
    Blocker.liftExecutionContext(ExecutionContexts.synchronous) // just for testing
  )

  def insertLog(jsonLog: Json): F[Unit] =
    sql"""insert into logs(data) values ($jsonLog);""".update.run.transact(xa).unsafeRunSync()
}

object LogManager {
  def apply[F[_]: Monad](): LogManager[F] = new LogManager[F]
}
