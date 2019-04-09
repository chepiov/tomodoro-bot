package org.chepiov.tomodoro.algebra

import org.chepiov.tomodoro.algebra.Telegram.{TMessage, TUser}

trait Pomodoro[F[_]] {
  def handleMessage(message: TMessage): F[Unit]
  def getInfo: F[TUser]
}
