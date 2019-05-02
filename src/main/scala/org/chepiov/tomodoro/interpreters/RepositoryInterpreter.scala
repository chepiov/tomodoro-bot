package org.chepiov.tomodoro.interpreters

import java.time.{Instant, OffsetDateTime, ZoneOffset}
import java.util.UUID

import cats.effect.{Async, ContextShift, Resource, Sync}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import doobie._
import doobie.hikari.HikariTransactor
import doobie.implicits._
import doobie.postgres.implicits._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Repository
import org.chepiov.tomodoro.algebras.Repository.ActivityDescriptor.TomodoroFinished
import org.chepiov.tomodoro.algebras.Repository.{ActivityDescriptor, ActivityLog}
import org.flywaydb.core.Flyway

class RepositoryInterpreter[F[_]: Logger: Monad](xa: Transactor[F]) extends Repository[F] {
  import org.chepiov.tomodoro.interpreters.{RepositorySQL => SQL}

  override def findLogs(chatId: Long, offset: Long, limit: Long): F[List[ActivityLog]] =
    for {
      logs <- SQL.findLogs(chatId, offset, limit).to[List].transact(xa)
      _    <- Logger[F].debug(s"[$chatId] Found logs, limit: $limit, offset: $offset, size: ${logs.size}")
    } yield logs

  override def addLog(log: ActivityLog): F[Unit] =
    for {
      id <- SQL.addLog(log).withUniqueGeneratedKeys[UUID]("id").transact(xa)
      r  <- Logger[F].debug(s"[${log.chatId}] Log added with id: $id")
    } yield r

  override def countCompleted(chatId: Long, from: OffsetDateTime, to: OffsetDateTime): F[Long] =
    for {
      cnt <- SQL.countCompleted(chatId, from, to).unique.transact(xa)
      _   <- Logger[F].debug(s"[$chatId] Found $cnt completed tomodoroes between $from and $to")
    } yield cnt
}

case object RepositoryInterpreter {

  def apply[I[_]: Applicative, F[_]: Logger: Async: ContextShift](xa: Transactor[F]): I[Repository[F]] =
    (new RepositoryInterpreter[F](xa): Repository[F]).pure[I]

  def apply[F[_]: Async: ContextShift](xa: Transactor[F]): F[Repository[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create[F]
      r                            <- apply[F, F](xa)
    } yield r

  final case class DbConfig(
      driver: String,
      uri: String,
      user: String,
      password: String,
      poolSize: Int
  )

  def dbTransactor[F[_]: Async: ContextShift](
      config: DbConfig
  ): Resource[F, Transactor[F]] = {
    for {
      connEc <- ExecutionContexts.fixedThreadPool[F](config.poolSize)
      txnEc  <- ExecutionContexts.cachedThreadPool[F]
      xa <- HikariTransactor.newHikariTransactor[F](
             config.driver,
             config.uri,
             config.user,
             config.password,
             connEc,
             txnEc
           )
    } yield xa
  }

  def initializeDb[F[_]: Sync](config: DbConfig): F[Unit] =
    Sync[F].delay {
      val fw: Flyway = {
        Flyway
          .configure()
          .dataSource(config.uri, config.user, config.password)
          .load()
      }
      fw.migrate()
    }.as(())
}

case object RepositorySQL {

  implicit val odtMeta: Meta[OffsetDateTime] =
    Meta[Instant].timap(i => OffsetDateTime.ofInstant(i, ZoneOffset.UTC))(odt => odt.toInstant)

  implicit val statTypeMeta: Meta[ActivityDescriptor] =
    Meta[String].timap(ActivityDescriptor.withName)(_.entryName)

  def addLog(log: ActivityLog): Update0 =
    sql"""
         INSERT into user_log(chat_id, time, descriptor, log)
         VALUES(${log.chatId}, ${log.time}, ${log.descriptor}, ${log.log})
       """.update

  def findLogs(chatId: Long, offset: Long, limit: Long): Query0[ActivityLog] =
    sql"""
         SELECT chat_id, time, descriptor, log
         FROM user_log
         WHERE chat_id = $chatId
         ORDER BY time DESC
         LIMIT $limit
         OFFSET $offset
       """.query[ActivityLog]

  def countCompleted(chatId: Long, from: OffsetDateTime, to: OffsetDateTime): Query0[Long] =
    sql"""
         SELECT count(id)
         FROM user_log
         WHERE chat_id = $chatId
         AND time BETWEEN $from AND $to
         AND descriptor = ${TomodoroFinished: ActivityDescriptor}
       """.query[Long]
}
