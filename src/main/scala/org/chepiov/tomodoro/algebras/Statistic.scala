package org.chepiov.tomodoro.algebras

/**
  * Represents statistic service of Tomodoro bot.
  *
  * @tparam F effect
  */
trait Statistic[F[_]] {
  def getLog(chatId: Long, pageNum: Int): Seq[(String, String)]
}
