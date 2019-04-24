package org.chepiov.tomodoro.algebras

import java.time.OffsetDateTime

import org.chepiov.tomodoro.algebras.User.Log

/**
  * Represents repository of Tomodoro bot data.
  *
  * @tparam F effect
  */
trait Repository[F[_]] {

  /**
    * Finds user activity, ordered by time (desc).
    *
    * @param chatId user chat id
    * @param offset of logs
    * @param limit  of logs
    * @return
    */
  def findLogs(chatId: Long, offset: Int, limit: Int = 10): F[List[Log]]

  /**
    * Adds new activity log
    *
    * @param log to add
    */
  def addLog(log: Log): F[Unit]

  /**
    * Counts completed by user tomodoroes in time period.
    *
    * @param chatId user chat id
    * @param from   start period
    * @param to     end period
    */
  def countCompleted(chatId: Long, from: OffsetDateTime, to: OffsetDateTime): F[Int]
}
