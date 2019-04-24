package org.chepiov.tomodoro.interpreters.hooks

import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.{Applicative, ApplicativeError}
import org.chepiov.tomodoro.algebras.Telegram
import org.chepiov.tomodoro.algebras.Telegram.{TEditMessage, TSendMessage}

import scala.util.{Failure, Try}

class UserChat[F[_]: ApplicativeError[?[_], Throwable]](telegram: Telegram[F]) {
  def sayTo(msg: TSendMessage): F[Try[Unit]] = {
    telegram.sendMessage(msg) *> Applicative[F].pure(Try(())).handleErrorWith { e =>
      Applicative[F].pure(Failure(e))
    }
  }

  def sayTo(msg: TEditMessage): F[Try[Unit]] = {
    telegram.editMessageText(msg) *> Applicative[F].pure(Try(())).handleErrorWith { e =>
      Applicative[F].pure(Failure(e))
    }
  }
}
