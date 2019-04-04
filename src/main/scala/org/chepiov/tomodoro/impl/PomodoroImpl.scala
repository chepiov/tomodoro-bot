package org.chepiov.tomodoro.impl

import cats.syntax.option._
import org.chepiov.tomodoro.BotMessage
import org.chepiov.tomodoro.api.{Pomodoro, Telegram}

class PomodoroImpl[F[_]](telegram: Telegram[F]) extends Pomodoro[F] {
  override def handleMessage(message: BotMessage): Unit = {
    message.text.foreach {
      case "/help" => telegram.sendMessage(message.chat.id, Pomodoro.helpMessage, message.messageId.some)
      case _       => ()
    }
  }
}
