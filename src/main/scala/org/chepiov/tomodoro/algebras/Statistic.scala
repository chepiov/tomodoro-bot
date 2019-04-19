package org.chepiov.tomodoro.algebras

/**
  * Represents statistic service of Tomodoro bot.
  *
  * @tparam F effect
  */
trait Statistic[F[_]] {
  def getLog(chatId: Long, offset: Int, limit: Int): F[Seq[(Long, String)]]
  def getCompletedLastDay(chatId: Long): F[Int]
  def getCompletedLastWeek(chatId: Long): F[Int]
  def getCompletedLastMonth(chatId: Long): F[Int]
}
