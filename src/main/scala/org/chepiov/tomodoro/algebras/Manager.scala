package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.Telegram.TMessage
import simulacrum.typeclass

/**
  * Represents manager of Tomodoro bot.
  *
  * @tparam F effect
  */
@typeclass
trait Manager[F[_]] {

  /**
    * Decides what Tomodoro bot should do.
    *
    * @param message incoming message from Telegram API.
    */
  def decide(message: TMessage): F[Unit]
}
