package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.UserSettings

trait Users[F[_]] {
  def getOrCreateUser(chatId: Long): F[User[F]]
}

case object Users {
  val defaultUserSettings = UserSettings(duration = 25, shortBreak = 4, longBreak = 20, amount = 4)
}
