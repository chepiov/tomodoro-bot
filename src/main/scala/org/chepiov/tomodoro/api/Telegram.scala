package org.chepiov.tomodoro.api

trait Telegram[F[_]] {
  def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long] = None): F[Unit]
}