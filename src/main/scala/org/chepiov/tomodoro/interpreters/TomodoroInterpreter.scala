package org.chepiov.tomodoro.interpreters

import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, MonadError}
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.{Manager, Telegram, Tomodoro}

class TomodoroInterpreter[F[_]: Logger: MonadError[?[_], Throwable]](manager: Manager[F], telegram: Telegram[F])
    extends Tomodoro[F] {

  override def handleUpdate(update: TUpdate): F[Unit] = {
    val result = for {
      _ <- Logger[F].debug(s"Received update: $update")
      r <- if (update.message.isDefined)
            manager.decide(update.message.get)
          else
            for {
              u <- Logger[F].debug("Empty update")
            } yield u
    } yield r
    result.handleErrorWith { e =>
      for {
        _ <- Logger[F].error(e)("Error during handling update")
        r <- e.raiseError[F, Unit]
      } yield r
    }
  }
  override def getInfo: F[TUser] =
    for {
      _ <- Logger[F].debug("Received info request")
      r <- telegram.getMe
    } yield r

  override def setWebHook(updateUrl: String): F[Unit] = ???

  override def deleteWebHook(): F[Unit] = ???
}

case object TomodoroInterpreter {
  def apply[I[_]: Monad, F[_]: Logger: MonadError[?[_], Throwable]](
      manager: Manager[F],
      telegram: Telegram[F]
  ): I[Tomodoro[F]] =
    for {
      _ <- Monad[I].unit
      t = new TomodoroInterpreter[F](manager, telegram)
    } yield t

  def apply[F[_]: Logger: MonadError[?[_], Throwable]](
      manager: Manager[F],
      telegram: Telegram[F]
  ): F[Tomodoro[F]] = apply[F, F](manager, telegram)
}
