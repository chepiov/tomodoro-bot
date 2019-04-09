package org.chepiov.tomodoro.algebra

trait Logger[F[_]] {
  def info(message: => String): F[Unit]
  def debug(message: => String): F[Unit]
  def warn(message: => String): F[Unit]
  def error(e: Throwable)(message: => String): F[Unit]
}
