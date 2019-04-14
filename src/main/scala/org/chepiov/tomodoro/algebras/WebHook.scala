package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.Telegram.TUpdate

trait WebHook[F[_]] {
  def handleUpdate(update: TUpdate): F[Boolean]
  def setWebHook(updateUrl: String): F[Unit]
  def deleteWebHook(): F[Unit]
}
