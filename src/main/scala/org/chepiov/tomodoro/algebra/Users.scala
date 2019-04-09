package org.chepiov.tomodoro.algebra

trait Users[F[_]] {
  def getOrCreateUser(chatId: Long): F[User[F]]
}
