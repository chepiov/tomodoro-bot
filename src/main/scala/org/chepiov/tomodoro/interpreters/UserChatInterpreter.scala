package org.chepiov.tomodoro.interpreters

import cats.effect.Sync
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Monad, MonadError}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.algebras.{Telegram, UserChat}

class UserChatInterpreter[F[_]: Logger: MonadError[?[_], Throwable]](telegram: Telegram[F]) extends UserChat[F] {

  override def sayTo(chatId: Long, msg: TSendMessage): F[Boolean] = {
    val result = for {
      _ <- Logger[F].debug(s"[$chatId] Saying to chat")
      r <- telegram.sendMessage(msg)
    } yield r

    result.map(_ => true).handleErrorWith { e =>
      for {
        _ <- Logger[F].error(e)(s"[$chatId] Error during saying to chat")
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

  def apply[F[_]: Sync](telegram: Telegram[F]): F[UserChat[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      uc                           <- apply[F, F](telegram)
    } yield uc
}
