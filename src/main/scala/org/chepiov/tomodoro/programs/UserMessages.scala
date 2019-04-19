package org.chepiov.tomodoro.programs

import java.time.OffsetDateTime

import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User._

import scala.concurrent.duration._

object UserMessages {

  def helpMsg(chatId: Long, settings: UserSettings): TSendMessage =
    TSendMessage(chatId, helpText(settings))

  def stateMsg(chatId: Long, state: UserState): TSendMessage =
    TSendMessage(chatId, stateText(state))

  def alreadyWorkingMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already working", none).some

  def alreadyBreakingMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already taking rest", none).some

  def alreadySuspendedMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already in pause", none).some

  def workingMsg(chatId: Long, remaining: Int, afterPause: Boolean): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""Tomodoro ${if (afterPause) "continued" else "started"}, remaining: $remaining""",
      progressKeyboard
    ).some

  def breakingMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(chatId, s"""You have a ${if (remaining == 0) "long" else "short"} break""", progressKeyboard).some

  def breakingAfterPauseMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(chatId, s"""Continuing a ${if (remaining == 0) "long" else "short"} break""", progressKeyboard).some

  def waitingWorkMsg(chatId: Long, remaining: Int, cycleStart: Boolean): Option[TSendMessage] =
    TSendMessage(chatId, s"""Break finished, remaining: $remaining""", waitingKeyboard(cycleStart)).some

  def waitingBreakMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(chatId, s"""Tomodoro finished, remaining: $remaining""", waitingKeyboard(false)).some

  def suspendedMsg(chatId: Long, work: Boolean): Option[TSendMessage] =
    TSendMessage(chatId, s"""${if (work) "Tomodoro" else "Break"} paused""", waitingKeyboard(false)).some

  def setSettingsMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, s"Choose type of setting:", setSettingsKeyboard).some

  def resetMsg(chatId: Long, settings: UserSettings): Option[TSendMessage] =
    TSendMessage(chatId, s"""Tomodoro reset, Your settings: ${settingsText(settings)}""", waitingKeyboard(true)).some

  def durationMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Input new duration (in minutes)", none).some

  def longBreakMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Input new long break duration (in minutes)", none).some

  def shortBreakMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Input new short break duration (in minutes)", none).some

  def amountMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Input new amount of tomodoroes in the cycle", none).some

  def settingsUpdatedMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Settings updated").some

  def invalidValueMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Value must be > 0").some

  private val downButtons: List[TKeyboardButton] =
    List(TKeyboardButton("/settings"), TKeyboardButton("/stats"), TKeyboardButton("/help"), TKeyboardButton("/about"))

  private val progressKeyboard: Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(
      List(List(TKeyboardButton("/pause"), TKeyboardButton("/skip"), TKeyboardButton("/reset")), downButtons)
    ).some

  val SettingsDurationData   = "settings_duration"
  val SettingsLongBreakData  = "settings_long_break"
  val SettingsShortBreakData = "settings_short_break"
  val SettingsAmountData     = "settings_amount"

  private val setSettingsKeyboard: Option[TInlineKeyboardMarkup] =
    TInlineKeyboardMarkup(
      List(
        List(
          TInlineKeyboardButton("Duration", SettingsDurationData),
          TInlineKeyboardButton("Long break", SettingsLongBreakData),
          TInlineKeyboardButton("Short break", SettingsShortBreakData),
          TInlineKeyboardButton("Amount", SettingsAmountData)
        )
      )
    ).some

  private def waitingKeyboard(cycleStart: Boolean): Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(List(List(TKeyboardButton(s"""${if (cycleStart) "/start" else "/continue"}""")), downButtons)).some

  private def settingsText(settings: UserSettings): String =
    s"""
       | Tomodoro duration: ${settings.duration} minutes
       | Short break duration: ${settings.shortBreak} minutes
       | Long break duration: ${settings.longBreak} minutes
       | Amount of tomodoroes in cycle: ${settings.amount} tomodoroes
       |""".stripMargin

  private def helpText(settings: UserSettings): String =
    s"""
      |*Available commands:*
      | `/help`      - show help information
      | `/state`     - show current state
      | `/start`     - start tomodoro or start a break 
      | `/continue`  - same as `/start`
      | `/pause`     - pause tomodoro or pause a break
      | `/skip`      - skip current tomodoro or current break
      | `/reset`     - reset the whole tomodoro cycle
      | `/settings`  - change settings
      | `/stats`     - show stats
      | 
      | *Your current settings:*
      |${settingsText(settings)} 
    """.stripMargin

  private def remainingMinutes(endTime: Long): FiniteDuration = {
    val remaining = FiniteDuration(math.max(0, endTime - OffsetDateTime.now().toEpochSecond), SECONDS).toMinutes
    Duration.create(remaining, MINUTES)
  }

  private def minutesGone(suspendTime: Long): FiniteDuration = {
    val suspended = FiniteDuration(OffsetDateTime.now().toEpochSecond - suspendTime, SECONDS).toMinutes
    Duration.create(suspended, MINUTES)
  }

  private def stateText(state: UserState): String = {
    state.status match {
      case WaitingWork(_, _) =>
        "You are ready to start tomodoro, press */start* or */continue*"
      case WaitingBreak(_, _) =>
        "You are ready to taking rest, press */start* or */continue*"
      case Working(remaining, _, endTime) =>
        s"You are working, remaining tomodoroes in the current cycle: $remaining, you will finish in ${remainingMinutes(endTime)}"
      case Breaking(_, _, endTime) =>
        s"You are taking rest, you will finish in ${remainingMinutes(endTime)}"
      case WorkSuspended(_, _, suspend) =>
        s"You paused tomodoro ${minutesGone(suspend)} ago"
      case BreakSuspended(_, _, suspend) =>
        s"You paused your break ${minutesGone(suspend)} ago"
    }
  }
}
