package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.UserStatsResult

/**
  * Represents statistic service of Tomodoro bot.
  *
  * @tparam F effect
  */
trait Statistic[F[_]] {
  def getLog(chatId: Long, page: Int): F[UserStatsResult]
  def getCompletedLastDay(chatId: Long): F[UserStatsResult]
  def getCompletedLastWeek(chatId: Long): F[UserStatsResult]
  def getCompletedLastMonth(chatId: Long): F[UserStatsResult]
}
