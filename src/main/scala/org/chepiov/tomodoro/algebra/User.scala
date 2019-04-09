package org.chepiov.tomodoro.algebra

import org.chepiov.tomodoro.algebra.Telegram.{Info, Settings}

trait User[F[_]] {
  def getSettings: F[Settings]
  def updateSettings(s: Settings): F[Unit]
  def run: F[Either[String, Info]]
  def rerun: F[Either[String, Info]]
  def pause: F[Either[String, Info]]
  def stop: F[Either[String, Info]]
}
