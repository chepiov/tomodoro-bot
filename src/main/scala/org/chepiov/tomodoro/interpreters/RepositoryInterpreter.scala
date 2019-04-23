package org.chepiov.tomodoro.interpreters

import java.time.{Instant, OffsetDateTime, ZoneOffset}
import java.util.UUID

import cats.Applicative
import cats.effect.{Async, ContextShift}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import doobie.util.transactor.Transactor.Aux
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Repository

class RepositoryInterpreter[F[_]: Logger: Async: ContextShift](xa: Aux[F, Unit]) extends Repository[F] {
  import org.chepiov.tomodoro.interpreters.{RepositorySQL => SQL}

  override def findChatActivity(chatId: Long, limit: Int, offset: Int): F[Seq[(Long, String)]] =
    Seq((1L, "")).pure[F]

  override def addLog(chatId: Long, time: OffsetDateTime, descriptor: String, log: String): F[Unit] =
    for {
      id <- SQL.addLog(chatId, time, descriptor, log).withUniqueGeneratedKeys[UUID]("id").transact(xa)
      r  <- Logger[F].debug(s"[$chatId] Log added with id: $id")
    } yield r

}

case object RepositoryInterpreter {

  def apply[I[_]: Applicative, F[_]: Logger: Async: ContextShift](config: DbConfig): I[Repository[F]] =
    for {
      _ <- Applicative[I].unit
      xa = Transactor.fromDriverManager[F](
        config.driver,
        config.uri,
        config.user,
        config.password
      )
    } yield new RepositoryInterpreter[F](xa)

  def apply[F[_]: Async: ContextShift](config: DbConfig): F[Repository[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create[F]
      r                            <- apply[F, F](config)
    } yield r

  final case class DbConfig(driver: String, uri: String, user: String, password: String)
}

case object RepositorySQL {

  implicit val odtMeta: Meta[OffsetDateTime] =
    Meta[Instant].timap(i => OffsetDateTime.ofInstant(i, ZoneOffset.UTC))(odt => odt.toInstant)

  def addLog(chatId: Long, time: OffsetDateTime, descriptor: String, log: String): Update0 =
    sql"""
         INSERT into user_log(chat_id, time, descriptor, log)
         VALUES($chatId, $time, $descriptor, $log)
       """.update
}
