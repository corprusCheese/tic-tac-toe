package tictactoe.logs

import cats._
import cats.effect._
import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import doobie._
import doobie.implicits._
import doobie.util.transactor.Transactor.Aux
import io.circe.Json
import io.circe.parser._
import org.postgresql.util.PGobject

class LogManager[F[_]: Monad: BracketThrow: Async: ContextShift] {
  val config: Config = ConfigFactory
    .load("config/reference")
    .getConfig("tictactoe")

  val xa: Aux[F, Unit] = Transactor.fromDriverManager[F](
    "org.postgresql.Driver",                                    // driver classname
    config.getString("dbConnectUrl"),                           // connect URL (driver-specific)
    config.getString("dbUser"),                                 // user
    config.getString("dbPassword"),                             // password
    Blocker.liftExecutionContext(ExecutionContexts.synchronous) // just for testing
  )

  implicit val jsonMeta: Meta[Json] =
    Meta.Advanced
      .other[PGobject]("json")
      .timap[Json](a => parse(a.getValue).leftMap[Json](e => throw e).merge) { a =>
        val o = new PGobject
        o.setType("json")
        o.setValue(a.noSpaces)
        o
      }

  def insertLog(jsonLog: Json): F[Unit] =
    MonadThrow[F].handleErrorWith(
      sql"""insert into logs(data, created) values ($jsonLog, now());""".update.run
        .transact(xa)
        .map(_ => ())
    )({ err: Throwable =>
      println(err).pure[F]
    })
}

object LogManager {
  def apply[F[_]: Monad: BracketThrow: Async: ContextShift](): LogManager[F] = new LogManager[F]
}
