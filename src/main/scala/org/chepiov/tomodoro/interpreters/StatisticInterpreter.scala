package org.chepiov.tomodoro.interpreters

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Monad}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.{Repository, Statistic}

class StatisticInterpreter[F[_]: Logger: Monad](repository: Repository[F]) extends Statistic[F] {

  private val pageSize = 10

  override def getLog(chatId: Long, page: Int): F[UserStatsResult] =
    for {
      _    <- Logger[F].debug(s"[$chatId] Finding user log, page: $page")
      logs <- repository.findLogs(chatId, pageSize * page, pageSize)
    } yield PushLog(page, logs)

  override def getCompletedLastDay(chatId: Long): F[UserStatsResult] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented")
    } yield PushCompletedLastDay

  override def getCompletedLastWeek(chatId: Long): F[UserStatsResult] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented")
    } yield PushCompletedLastWeek

  override def getCompletedLastMonth(chatId: Long): F[UserStatsResult] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented")
    } yield PushCompletedLastMonth
}

case object StatisticInterpreter {

  def apply[I[_]: Applicative, F[_]: Logger: Monad](repository: Repository[F]): I[Statistic[F]] =
    (new StatisticInterpreter[F](repository): Statistic[F]).pure[I]

  def apply[F[_]: Sync](repository: Repository[F]): F[Statistic[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      s                            <- apply[F, F](repository)
    } yield s
}
