package org.chepiov.tomodoro.interpreters

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import org.chepiov.tomodoro.algebra.Telegram.{Settings, TMessage, TUser}
import org.chepiov.tomodoro.algebra.{Logger, Pomodoro, Telegram}

class PomodoroInterpreter[F[_]: Monad](telegram: Telegram[F], logger: Logger[F]) extends Pomodoro[F] {

  override def handleMessage(message: TMessage): F[Unit] = {
    message.text.getOrElse("") match {
      case "/help" =>
        for {
          _ <- logger.debug("Received help request message")
          _ <- sendHelp(message.chat.id, message.messageId)
        } yield ()
      case "/run" =>
        for {
          _ <- logger.debug("Received run request message")
          _ <- telegram.run(message.chat.id)
        } yield ()
      case "/rerun" =>
        for {
          _ <- logger.debug("Received run request message")
          _ <- telegram.custom(message.chat.id, "rerun")
        } yield ()
      case "/end" =>
        for {
          _ <- logger.debug("Received run request message")
          _ <- telegram.end(message.chat.id)
        } yield ()
      case "/settings" =>
        for {
          _ <- logger.debug("Received run request message")
          _ <- telegram.settings(message.chat.id, Settings(4, 20, 4))
        } yield ()
      case m =>
        for {
          _ <- logger.warn(s"Received unknown message: $m")
          _ <- sendHelp(message.chat.id, message.messageId)
        } yield ()
    }
  }

  override def getInfo: F[TUser] =
    telegram.me()

  private def sendHelp(chatId: Long, messageId: Long): F[Unit] =
    telegram.help(chatId, messageId.some)
}

object PomodoroInterpreter {
  def apply[F[_]: Monad](telegram: Telegram[F], logger: Logger[F]): F[Pomodoro[F]] =
    Monad[F].pure(new PomodoroInterpreter(telegram, logger))
}
