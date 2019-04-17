package org.chepiov.tomodoro.algebras

import org.chepiov.tomodoro.algebras.Telegram.{TUpdate, TUser}
import simulacrum.typeclass

/**
  * Represents Tomodoro bot.
  *
  * @tparam F effect
  */
@typeclass
trait Tomodoro[F[_]] {

  /**
    * Handles incoming update from Telegram API.
    *
    * @param update to handle
    * @return true if update was handled
    */
  def handleUpdate(update: TUpdate): F[Unit]

  /**
    * Returns info about Tomodoro.
    *
    * @return info
    */
  def getInfo: F[TUser]

  def setWebHook(updateUrl: String): F[Unit]

  def deleteWebHook(): F[Unit]
}
