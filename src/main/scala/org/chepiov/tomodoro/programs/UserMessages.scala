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
    TSendMessage(chatId, "You are already working, ignoring", none).some

  def alreadyBreakingMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already taking rest, ignoring", none).some

  def alreadySuspendedMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already in pause, ignoring", none).some

  def cantSkipMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Nothing to skip, ignoring", none).some

  def workingMsg(chatId: Long, remaining: Int, endTime: Long, afterPause: Boolean): Option[TSendMessage] =
    TSendMessage(
      chatId,
      workingText(afterPause, remaining, endTime),
      progressKeyboard
    ).some

  def breakingMsg(chatId: Long, remaining: Int, endTime: Long): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""You have a ${if (remaining == 0) "*long*" else "*short*"} break, ends in ${remainingMinutes(endTime)}""".stripMargin,
      progressKeyboard
    ).some

  def breakingAfterPauseMsg(chatId: Long, remaining: Int, endTime: Long): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""*Continuing* a ${if (remaining == 0) "long" else "short"} break, ends in ${remainingMinutes(endTime)}""",
      progressKeyboard
    ).some

  def waitingWorkMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""*Break* finished, remaining tomodoroes in the current cycle: $remaining, say */continue* when you're ready""",
      waitingKeyboard()
    ).some

  def waitingBreakMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""*Tomodoro* finished, remaining  tomodoroes in the current cycle: $remaining, say */continue* for taking rest""",
      waitingKeyboard()
    ).some

  def suspendedMsg(chatId: Long, work: Boolean): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""${if (work) "*Tomodoro*" else "*Break*"} paused, say */continue* when you're ready""",
      waitingKeyboard()
    ).some

  def setSettingsMsg(chatId: Long, settings: UserSettings): Option[TSendMessage] =
    TSendMessage(chatId, s"${settingsText(settings)} Select type of setting you wish to update", setSettingsKeyboard).some

  def statsMsg(chatId: Long): TSendMessage =
    TSendMessage(chatId, s"Select type of report", statsKeyboard)

  def resetMsg(chatId: Long, settings: UserSettings): Option[TSendMessage] =
    TSendMessage(chatId, s"""*Cycle reset*, your current settings: ${settingsText(settings)}""", waitingKeyboard()).some

  def durationMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Say new *duration* (in minutes)", none).some

  def longBreakMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Say new *long break* duration (in minutes)", none).some

  def shortBreakMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Say new *short break* duration (in minutes)", none).some

  def amountMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Say new *amount* of tomodoroes in the cycle", none).some

  def settingsUpdatedMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Settings updated").some

  def invalidValueMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "Must be > 0").some

  private val downButtons: List[TKeyboardButton] =
    List(TKeyboardButton("/state"), TKeyboardButton("/reset"), TKeyboardButton("/settings"), TKeyboardButton("/stats"))

  private val progressKeyboard: Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(
      List(List(TKeyboardButton("/pause"), TKeyboardButton("/skip")), downButtons)
    ).some

  val SettingsDurationData   = "settings_duration"
  val SettingsLongBreakData  = "settings_long_break"
  val SettingsShortBreakData = "settings_short_break"
  val SettingsAmountData     = "settings_amount"

  val StatsLogData           = "stats_log"
  val StatsCountPerDayData   = "stats_count_per_day"
  val StatsCountPerWeekData  = "stats_count_per_week"
  val StatsCountPerMonthData = "stats_count_per_month"

  private val setSettingsKeyboard: Option[TInlineKeyboardMarkup] =
    TInlineKeyboardMarkup(
      List(
        List(
          TInlineKeyboardButton("Duration", SettingsDurationData),
          TInlineKeyboardButton("Long break", SettingsLongBreakData)
        ),
        List(
          TInlineKeyboardButton("Short break", SettingsShortBreakData),
          TInlineKeyboardButton("Amount", SettingsAmountData)
        )
      )
    ).some

  private val statsKeyboard: Option[TInlineKeyboardMarkup] =
    TInlineKeyboardMarkup(
      List(
        List(
          TInlineKeyboardButton("Activity log", StatsLogData),
          TInlineKeyboardButton("Last day completed", StatsCountPerDayData)
        ),
        List(
          TInlineKeyboardButton("Last week completed", StatsCountPerWeekData),
          TInlineKeyboardButton("Last month completed", StatsCountPerMonthData)
        )
      )
    ).some

  private def waitingKeyboard(): Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(List(List(TKeyboardButton(s"/continue")), downButtons)).some

  private def workingText(afterPause: Boolean, remaining: Int, endTime: Long): String =
    s"""
      |${if (afterPause) "*Continuing*" else "*Starting*"}
      |Remaining tomodoroes in the current cycle: $remaining
      |Ends in ${remainingMinutes(endTime)}
    """.stripMargin

  private def settingsText(settings: UserSettings): String =
    s"""
      | Tomodoro duration: ${settings.duration} minutes
      | Short break duration: ${settings.shortBreak} minutes
      | Long break duration: ${settings.longBreak} minutes
      | Amount of tomodoroes in cycle: ${settings.amount} tomodoroes
      |
    """.stripMargin

  private def helpText(settings: UserSettings): String =
    s"""
      |*About pomodoro technique:* [Wikipedia](https://en.wikipedia.org/wiki/Pomodoro_Technique)
      |
      |*Available commands:*
      | `/help`      - show help information
      | `/state`     - show current state
      | `/continue`  - start tomodoro or start a break 
      | `/pause`     - pause tomodoro or pause a break
      | `/skip`      - skip current tomodoro or current break
      | `/reset`     - reset the whole tomodoro cycle
      | `/settings`  - change settings
      | `/stats`     - show stats
      | 
      | *Your current settings:*${settingsText(settings)} 
    """.stripMargin

  private def stateText(state: UserState): String = {
    state.status match {
      case WaitingWork(_, _) =>
        s"""
          |You are ready to start tomodoro, press */continue*
          |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case WaitingBreak(_, _) =>
        s"""
          |You are ready to taking rest, press */continue*
          |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case Working(r, _, t) =>
        s"""
          |You are working, remaining tomodoroes in the current cycle: $r, you will finish in ${remainingMinutes(t)}
          |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case Breaking(_, _, endTime) =>
        s"""
          |You are taking rest, you will finish in ${remainingMinutes(endTime)}
          |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case WorkSuspended(_, _, suspend) =>
        s"""
          |You paused tomodoro ${minutesGone(suspend)} ago
          |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case BreakSuspended(_, _, suspend) =>
        s"""
          |You paused your break ${minutesGone(suspend)} ago
          |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
    }
  }

  private def remainingMinutes(endTime: Long): FiniteDuration = {
    val remaining = FiniteDuration(math.max(0, endTime - OffsetDateTime.now().toEpochSecond), SECONDS).toMinutes
    Duration.create(remaining, MINUTES)
  }

  private def minutesGone(suspendTime: Long): FiniteDuration = {
    val suspended = FiniteDuration(OffsetDateTime.now().toEpochSecond - suspendTime, SECONDS).toMinutes
    Duration.create(suspended, MINUTES)
  }
}
