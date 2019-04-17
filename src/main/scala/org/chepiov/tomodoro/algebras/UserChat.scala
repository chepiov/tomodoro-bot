package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.User.Answer
import simulacrum.typeclass

/**
  * Represents chat with user.
  *
  * @tparam F effect
  */
@typeclass
trait UserChat[F[_]] {

  /**
    * Says to chat.
    *
    * @param chatId  user chat id
    * @param message message to say
    * @return true if success
    */
  def sayTo(chatId: Long, message: Answer): F[Boolean]
}
