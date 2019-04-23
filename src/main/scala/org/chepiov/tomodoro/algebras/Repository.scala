package org.chepiov.tomodoro.algebras

import java.time.OffsetDateTime

/**
  * Represents repository of Tomodoro bot data.
  *
  * @tparam F effect
  */
trait Repository[F[_]] {
  def findChatActivity(chatId: Long, limit: Int, offset: Int): F[Seq[(Long, String)]]
  def addLog(chatId: Long, time: OffsetDateTime, descriptor: String, log: String): F[Unit]
}
