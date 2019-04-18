package org.chepiov.tomodoro.algebras

/**
  * Represents repository of Tomodoro bot data.
  *
  * @tparam F effect
  */
trait Repository[F[_]] {
  def findChatActivity(chatId: Long, limit: Int, offset: Int): Seq[(String, String)]
}
