package org.chepiov.tomodoro.api

import org.chepiov.tomodoro.{BotMessage, BotUser}

trait Pomodoro[F[_]] {
  def handleMessage(message: BotMessage): F[Unit]
  def getInfo: F[BotUser]
}

case object Pomodoro {
  val helpMessage: String =
    """
      |*Commands:*
      |`/help` - this command
      |`/run` - starts pomodoro
      |`/pause` - pauses current pomodoro
      |`/stop` - stops current pomodoro
      |`/rerun` - stops current pomodoro and starts new one
      |`/set p <time>` - sets duration of pomodoro (in minutes)
      |`/set l <time>` - sets duration of long break (in minutes)
      |`/set s <time>` - sets duration of short break (in minutes)
      |`/set c <pomodoro_amount>` - sets amount of pomodoros between the long breaks
      |`/stats <period>` - shows statistic
    """.stripMargin
}
