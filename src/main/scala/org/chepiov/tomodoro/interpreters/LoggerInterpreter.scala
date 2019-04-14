package org.chepiov.tomodoro.interpreters

import cats.effect.Sync
import io.chrisdavenport.log4cats.SelfAwareStructuredLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Logger

class LoggerInterpreter[F[_]: Sync](name: String) extends Logger[F] {

  implicit def logger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLoggerFromName(name)

  override def info(message: => String): F[Unit] =
    logger.info(message)

  override def debug(message: => String): F[Unit] =
    logger.debug(message)

  override def warn(message: => String): F[Unit] =
    logger.warn(message)

  override def error(e: Throwable)(message: => String): F[Unit] =
    logger.error(e)(message)
}

case object LoggerInterpreter {
  def apply[F[_]: Sync](name: String): F[Logger[F]] =
    Sync[F].delay(new LoggerInterpreter(name))
}
