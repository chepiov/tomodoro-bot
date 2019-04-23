package org.chepiov.tomodoro.interpreters

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import cats.Applicative
import cats.effect.Effect
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.{Repository, Statistic}

class StatisticInterpreter[F[_]: Logger: Effect](system: ActorSystem, repository: Repository[F]) extends Statistic[F] {

  implicit val mat: Materializer = ActorMaterializer()(system)

  override def getLog(chatId: Long, offset: Int, limit: Int): F[Seq[(Long, String)]] =
    for {
      r <- repository.findChatActivity(chatId, limit, offset)
      _ <- Logger[F].debug(s"[$chatId] Not yet implemented, stub: $r")
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

  def apply[I[_]: Applicative, F[_]: Logger: Effect](system: ActorSystem, repository: Repository[F]): I[Statistic[F]] =
    (new StatisticInterpreter[F](system, repository): Statistic[F]).pure[I]

  def apply[F[_]: Effect](system: ActorSystem, repository: Repository[F]): F[Statistic[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      s                            <- apply[F, F](system, repository)
    } yield s
}
