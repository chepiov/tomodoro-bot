package org.chepiov.tomodoro.algebra

trait Telegram[F[_]] {
  import Telegram._

  def help(chatId: Long, messageIdReplyTo: Option[Long] = None): F[Unit]
  def run(chatId: Long): F[Unit]
  def end(chatId: Long): F[Unit]
  def settings(chatId: Long, settings: Settings, messageIdReplyTo: Option[Long] = None): F[Unit]
  def custom(chatId: Long, message: String, messageIdReplyTo: Option[Long] = None): F[Unit]
  def me(): F[TUser]
}

case object Telegram {

  final case class Info(pomodoro: Int, remaining: Int)
  final case class Settings(shortBreak: Int, longBreak: Int, amount: Int)

  final case class TUpdate(updateId: Long, message: Option[TMessage])
  final case class TMessage(messageId: Long, chat: TChat, text: Option[String])
  final case class TChat(id: Long)
  final case class TUser(
      id: Long,
      isBot: Boolean,
      firstName: String,
      lastName: Option[String],
      userName: Option[String],
      languageCode: Option[String]
  )
  final case class TResponse[A](ok: Boolean, result: A)

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

  val ranMessage: String =
    """
    | Pomodoro started.
  """.stripMargin

  val endedMessage: String =
    """
    | Pomodoro ended.
  """.stripMargin

  def settingsMessage(settings: Settings): String =
    s"""
      | Short break: ${settings.shortBreak} 
      | Long break:  ${settings.longBreak} 
      | Amount:      ${settings.amount} 
    """.stripMargin
}
