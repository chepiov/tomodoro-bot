package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.UserSettings
import simulacrum.typeclass

/**
  * Represents all Tomodoro bot users.
  *
  * @tparam F effect
  */
@typeclass
trait Users[F[_]] {

  /**
    * Returns user by chat id or creates the new one.
    *
    * @param chatId to use
    * @return user
    */
  def getOrCreateUser(chatId: Long): F[User[F]]
}

case object Users {

  /**
    * Default user settings for new users.
    */
  val defaultUserSettings = UserSettings(duration = 25, shortBreak = 4, longBreak = 20, amount = 3)
}
