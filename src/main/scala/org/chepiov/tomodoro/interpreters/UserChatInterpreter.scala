package org.chepiov.tomodoro.interpreters

import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, MonadError}
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.algebras.{Telegram, UserChat}

class UserChatInterpreter[F[_]: Logger: MonadError[?[_], Throwable]](telegram: Telegram[F]) extends UserChat[F] {

  override def sayTo(chatId: Long, msg: TSendMessage): F[Boolean] = {
    val result = for {
      _ <- Logger[F].debug(s"[$chatId] Message: $msg")
      r <- telegram.sendMessage(msg)
    } yield r

    result.map(_ => true).handleErrorWith { e =>
      for {
        _ <- Logger[F].error(e)(s"Error during saying to chat: $chatId")
      } yield false
    }
  }
}

case object UserChatInterpreter {

  def apply[I[_]: Monad, F[_]: Logger: MonadError[?[_], Throwable]](telegram: Telegram[F]): I[UserChat[F]] =
    for {
      _ <- Monad[I].unit
      u = new UserChatInterpreter[F](telegram)
    } yield u

  def apply[F[_]: Logger: MonadError[?[_], Throwable]](telegram: Telegram[F]): F[UserChat[F]] =
    apply[F, F](telegram)
}
