package org.chepiov.tomodoro.algebras

import simulacrum.typeclass

/**
  * Represents statistic service of Tomodoro bot.
  *
  * @tparam F effect
  */
@typeclass
trait Statistic[F[_]] {
  def sendActivity(chatId: Long, page: Int, messageId: Option[Long] = None): F[Unit]
  def sendCompletedLastDay(chatId: Long): F[Unit]
  def sendCompletedLastWeek(chatId: Long): F[Unit]
  def sendCompletedLastMonth(chatId: Long): F[Unit]
}
