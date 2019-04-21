package org.chepiov.tomodoro.interpreters

import cats.Applicative
import cats.effect.Sync
import cats.syntax.functor._
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Statistic

class StatisticInterpreter[F[_]: Logger: Applicative] extends Statistic[F] {

  override def getLog(chatId: Long, offset: Int, limit: Int): F[Seq[(Long, String)]] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented, params: $offset, $limit")
    } yield Seq[(Long, String)]()

  override def getCompletedLastDay(chatId: Long): F[Int] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented")
    } yield 0

  override def getCompletedLastWeek(chatId: Long): F[Int] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented")
    } yield 0

  override def getCompletedLastMonth(chatId: Long): F[Int] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented")
    } yield 0
}

case object StatisticInterpreter {

  def apply[I[_]: Applicative, F[_]: Logger: Applicative](): I[Statistic[F]] =
    for {
      _ <- Applicative[I].unit
      s = new StatisticInterpreter[F]()
    } yield s

  def apply[F[_]: Sync](): F[Statistic[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      s <- apply[F, F]()
    } yield s
}
