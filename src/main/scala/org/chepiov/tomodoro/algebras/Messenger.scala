package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.Answer

trait Messenger[F[_]] {
  def sendToUser(chatId: Long, answer: Answer): F[Boolean]
}
