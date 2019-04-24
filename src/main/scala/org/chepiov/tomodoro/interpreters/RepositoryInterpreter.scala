package org.chepiov.tomodoro.interpreters

import java.time.{Instant, OffsetDateTime, ZoneOffset}
import java.util.UUID

import cats.effect.{Async, ContextShift}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Repository
import org.chepiov.tomodoro.algebras.User.Log

class RepositoryInterpreter[F[_]: Logger: Monad](xa: Transactor[F]) extends Repository[F] {
  import org.chepiov.tomodoro.interpreters.{RepositorySQL => SQL}

  override def findLogs(chatId: Long, offset: Int, limit: Int = 10): F[List[Log]] =
    for {
      logs <- SQL.findLogs(chatId, offset, limit).to[List].transact(xa)
      _    <- Logger[F].debug(s"[$chatId] Found logs, limit: $limit, offset: $offset, size: ${logs.size}")
    } yield logs

  override def addLog(log: Log): F[Unit] =
    for {
      id <- SQL.addLog(log).withUniqueGeneratedKeys[UUID]("id").transact(xa)
      r  <- Logger[F].debug(s"[${log.chatId}] Log added with id: $id")
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

  def addLog(log: Log): Update0 =
    sql"""
         INSERT into user_log(chat_id, time, descriptor, log)
         VALUES(${log.chatId}, ${log.time}, ${log.descriptor}, ${log.log})
       """.update

  def findLogs(chatId: Long, offset: Int, limit: Int): Query0[Log] =
    sql"""
         SELECT chat_id, time, descriptor, log
         FROM user_log
         WHERE chat_id = $chatId
         ORDER BY time DESC
         LIMIT $limit
         OFFSET $offset
       """.query[Log]
}
