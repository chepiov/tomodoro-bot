package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.Monad
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.chepiov.tomodoro.algebras.Telegram.TMessage
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.{Logger, Tomodoro, Users}

class TomodoroInterpreter[F[_]: MError](users: Users[F], logger: Logger[F]) extends Tomodoro[F] {
  import TomodoroInterpreter._

  override def handleMessage(message: TMessage): F[Boolean] = {
    val r = message.text.getOrElse("") match {
      case "/help" =>
        for {
          _    <- logger.debug("Received help request message")
          user <- users.getOrCreateUser(message.chat.id)
          _    <- user.info(GetHelp)
        } yield true
      case "/continue" =>
        for {
          _    <- logger.debug(s"[${message.chat.id}] Received continue request message")
          user <- users.getOrCreateUser(message.chat.id)
          _    <- user.advance(Continue(now))
        } yield true
      case "/suspend" =>
        for {
          _    <- logger.debug(s"[${message.chat.id}] Received suspend request message")
          user <- users.getOrCreateUser(message.chat.id)
          _    <- user.advance(Suspend(now))
        } yield true
      case "/stop" =>
        for {
          _    <- logger.debug(s"[${message.chat.id}] Received stop request message")
          user <- users.getOrCreateUser(message.chat.id)
          _    <- user.advance(Stop(now))
        } yield true
      case m =>
        for {
          _    <- logger.warn(s"[${message.chat.id}] Received unknown message: $m")
          user <- users.getOrCreateUser(message.chat.id)
          _    <- user.info(GetHelp)
        } yield true
    }
    r handleErrorWith { e =>
      for {
        _ <- logger.error(e)(s"[${message.chat.id}] Error during handling message ")
      } yield false
    }
  }
}

object TomodoroInterpreter {
  def apply[F[_]: MError](users: Users[F], logger: Logger[F]): F[Tomodoro[F]] =
    Monad[F].pure { new TomodoroInterpreter(users, logger) }

  private def now: Long = OffsetDateTime.now().toEpochSecond
}
