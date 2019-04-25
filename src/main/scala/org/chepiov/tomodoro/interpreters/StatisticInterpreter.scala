package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime
import java.time.temporal.ChronoUnit

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.{Repository, Statistic, Telegram}
import org.chepiov.tomodoro.programs.UserMessages._

class StatisticInterpreter[F[_]: Logger: Monad](telegram: Telegram[F], repository: Repository[F]) extends Statistic[F] {

  private val pageSize = 10

  override def sendActivity(chatId: Long, page: Int, messageId: Option[Long] = None): F[Unit] =
    for {
      _    <- Logger[F].debug(s"[$chatId] Finding user activity, page: $page, messageId: $messageId")
      logs <- repository.findLogs(chatId, pageSize * page, pageSize)
      r <- messageId match {
            case Some(id) => telegram.editMessageText(logsEditMsg(chatId, id, page, logs))
            case None     => telegram.sendMessage(logsMsg(chatId, page, logs))
          }
    } yield r

  override def sendCompletedLastDay(chatId: Long): F[Unit] =
    for {
      _         <- Logger[F].debug(s"[$chatId] Counting completed last day")
      now       = OffsetDateTime.now()
      dayBefore = now.minus(1, ChronoUnit.DAYS)
      cnt       <- repository.countCompleted(chatId, dayBefore, now)
      r         <- telegram.sendMessage(completedLastDayMsg(chatId, cnt))
    } yield r

  override def sendCompletedLastWeek(chatId: Long): F[Unit] =
    for {
      _          <- Logger[F].debug(s"[$chatId] Counting completed last week")
      now        = OffsetDateTime.now()
      weekBefore = now.minus(1, ChronoUnit.WEEKS)
      cnt        <- repository.countCompleted(chatId, weekBefore, now)
      r          <- telegram.sendMessage(completedLastWeekMsg(chatId, cnt))
    } yield r

  override def sendCompletedLastMonth(chatId: Long): F[Unit] =
    for {
      _           <- Logger[F].debug(s"[$chatId] Counting completed last month")
      now         = OffsetDateTime.now()
      monthBefore = now.minus(1, ChronoUnit.MONTHS)
      cnt         <- repository.countCompleted(chatId, monthBefore, now)
      r           <- telegram.sendMessage(completedLastMonthMsg(chatId, cnt))
    } yield r
}

case object StatisticInterpreter {

  def apply[I[_]: Applicative, F[_]: Logger: Monad](telegram: Telegram[F], repository: Repository[F]): I[Statistic[F]] =
    (new StatisticInterpreter[F](telegram, repository): Statistic[F]).pure[I]

  def apply[F[_]: Sync](telegram: Telegram[F], repository: Repository[F]): F[Statistic[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      s                            <- apply[F, F](telegram, repository)
    } yield s
}
