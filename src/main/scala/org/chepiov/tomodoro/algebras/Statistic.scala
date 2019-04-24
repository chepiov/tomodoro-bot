package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.{CompletedLastDayResult, CompletedLastMonthResult, CompletedLastWeekResult, ActivityResult}

/**
  * Represents statistic service of Tomodoro bot.
  *
  * @tparam F effect
  */
trait Statistic[F[_]] {
  def getLog(chatId: Long, page: Int): F[ActivityResult]
  def getCompletedLastDay(chatId: Long): F[CompletedLastDayResult]
  def getCompletedLastWeek(chatId: Long): F[CompletedLastWeekResult]
  def getCompletedLastMonth(chatId: Long): F[CompletedLastMonthResult]
}
