package org.chepiov.tomodoro.api

import org.chepiov.tomodoro.BotUser

trait Telegram[F[_]] {
  def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long] = None): F[Unit]
  def me(): F[BotUser]
}
