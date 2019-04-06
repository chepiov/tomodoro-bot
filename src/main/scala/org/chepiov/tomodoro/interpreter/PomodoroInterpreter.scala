package org.chepiov.tomodoro.interpreter

import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebra.{Pomodoro, Telegram}
import org.chepiov.tomodoro.{BotMessage, BotUser}

class PomodoroInterpreter[F[_]: Sync](telegram: Telegram[F], logger: Logger[F]) extends Pomodoro[F] {

  override def handleMessage(message: BotMessage): F[Unit] = {
    message.text.getOrElse("") match {
      case "/help" =>
        for {
          _ <- logger.debug("Received help request message")
          _ <- sendHelp(message.chat.id, message.messageId)
        } yield ()
      case m =>
        for {
          _ <- logger.warn(s"Received unknown message: $m")
          _ <- sendHelp(message.chat.id, message.messageId)
        } yield ()
    }
  }

  override def getInfo: F[BotUser] =
    telegram.me()

  private def sendHelp(chatId: Long, messageId: Long): F[Unit] =
    telegram.sendMessage(chatId, Pomodoro.helpMessage, messageId.some)
}

object PomodoroInterpreter {
  def apply[F[_]: Sync](telegram: Telegram[F]): F[Pomodoro[F]] =
    Sync[F].delay(new PomodoroInterpreter(telegram, Slf4jLogger.getLogger[F]))
}
