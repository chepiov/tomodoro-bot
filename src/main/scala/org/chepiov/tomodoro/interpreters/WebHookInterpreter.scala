package org.chepiov.tomodoro.interpreters

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.{Logger, Tomodoro, WebHook}

class WebHookInterpreter[F[_]: MError](tomodoro: Tomodoro[F], logger: Logger[F]) extends WebHook[F] {

  override def handleUpdate(update: TUpdate): F[Boolean] =
    for {
      _ <- logger.debug(s"Received update: $update")
      result <- if (update.message.isDefined)
                 tomodoro.handleMessage(update.message.get)
               else Monad[F].pure(true)
    } yield result

  override def setWebHook(updateUrl: String): F[Unit] = ???

  override def deleteWebHook(): F[Unit] = ???
}

case object WebHookInterpreter {
  def apply[F[_]: MError](tomodoro: Tomodoro[F], logger: Logger[F]): F[WebHookInterpreter[F]] =
    Monad[F].pure(new WebHookInterpreter[F](tomodoro, logger))
}
