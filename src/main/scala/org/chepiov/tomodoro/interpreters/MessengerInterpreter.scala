package org.chepiov.tomodoro.interpreters

import cats.Applicative
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.chepiov.tomodoro.algebras.{Logger, Messenger, Telegram, User}

class MessengerInterpreter[F[_]: MError](telegram: Telegram[F], logger: Logger[F]) extends Messenger[F] {

  override def sendToUser(chatId: Long, answer: User.Answer): F[Boolean] =
    for {
      _ <- logger.debug(s"[$chatId] Answer: $answer")
      r <- telegram.help(chatId).map(_ => true).handleErrorWith { e =>
            for {
              _ <- logger.error(e)(s"[$chatId] Error during handling update")
            } yield false
          }
    } yield r
}

case object MessengerInterpreter {
  def apply[F[_]: MError](telegram: Telegram[F], logger: Logger[F]): F[Messenger[F]] =
    Applicative[F].pure(new MessengerInterpreter[F](telegram, logger))
}
