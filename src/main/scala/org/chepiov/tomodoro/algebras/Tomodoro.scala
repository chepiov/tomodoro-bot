package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.Telegram.TMessage

trait Tomodoro[F[_]] {
  def handleMessage(message: TMessage): F[Boolean]
}
