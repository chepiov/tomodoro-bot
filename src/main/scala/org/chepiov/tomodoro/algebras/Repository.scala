package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.Log

/**
  * Represents repository of Tomodoro bot data.
  *
  * @tparam F effect
  */
trait Repository[F[_]] {

  def findLogs(chatId: Long, offset: Int, limit: Int = 10): F[List[Log]]
  def addLog(log: Log): F[Unit]
}
