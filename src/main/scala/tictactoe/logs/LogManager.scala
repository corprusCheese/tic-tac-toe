package tictactoe.logs

import cats._
import cats.data.NonEmptyList
import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import doobie.{Transactor, _}
import doobie._
import doobie.implicits._
import doobie.util.transactor.Transactor.Aux
import io.circe._
import io.circe.parser._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.postgresql.util.PGobject

class LogManager[F[_]: Monad: BracketThrow: Async: ContextShift] {

  val connectUrl = "jdbc:postgresql://127.0.0.1:5432/tic-tac-toe"
  val user       = "postgres"
  val password   = "???"
  val xa: Aux[F, Unit] = Transactor.fromDriverManager[F](
    "org.postgresql.Driver",                                    // driver classname
    connectUrl,                                                 // connect URL (driver-specific)
    user,                                                       // user
    password,                                                   // password
    Blocker.liftExecutionContext(ExecutionContexts.synchronous) // just for testing
  )

  implicit val showPGobject: Show[PGobject] = Show.show(_.getValue.take(250))

  implicit val jsonGet: Get[Json] =
    Get.Advanced.other[PGobject](NonEmptyList.of("json")).temap[Json] { o =>
      parse(o.getValue).leftMap(_.show)
    }

  implicit val jsonPut: Put[Json] =
    Put.Advanced.other[PGobject](NonEmptyList.of("json")).tcontramap[Json] { j =>
      val o = new PGobject
      o.setType("json")
      o.setValue(j.noSpaces)
      o
    }

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
      sql"""insert into game.logs(data, created) values ($jsonLog, now());""".update.run
        .transact(xa)
        .map(_ => ())
    )({ err: Throwable =>
      println(err).pure[F]
    })
}

object LogManager {
  def apply[F[_]: Monad: BracketThrow: Async: ContextShift](): LogManager[F] = new LogManager[F]
}
