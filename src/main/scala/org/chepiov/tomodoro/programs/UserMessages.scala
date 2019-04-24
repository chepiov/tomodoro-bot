package org.chepiov.tomodoro.programs

import java.time.OffsetDateTime

import cats.syntax.option._
import enumeratum.{EnumEntry, _}
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User._

import scala.collection.immutable
import scala.concurrent.duration._

/**
  * User chat messages
  */
case object UserMessages {

  val stateSynonyms: Set[String]    = Set("/state", "state")
  val helpSynonyms: Set[String]     = Set("/help", "help", "/start")
  val statsSynonyms: Set[String]    = Set("/stats", "stats")
  val continueSynonyms: Set[String] = Set("/continue", "continue", "start", "take a break")
  val pauseSynonyms: Set[String]    = Set("/pause", "pause", "/suspend", "suspend")
  val resetSynonyms: Set[String]    = Set("/reset", "reset")
  val skipSynonyms: Set[String]     = Set("/skip", "skip")
  val settingsSynonyms: Set[String] = Set("/settings", "settings")

  sealed trait SettingsData extends EnumEntry {
    def cmd(time: Long): ChangeSettingsCommand
  }

  object SettingsData extends Enum[SettingsData] {

    override def values: immutable.IndexedSeq[SettingsData] = findValues

    object SettingsDurationData extends SettingsData {
      override def entryName: String                      = "settings_duration"
      override def cmd(time: Long): ChangeSettingsCommand = AwaitChangingDuration(time)
    }

    object SettingsLongBreakData extends SettingsData {
      override def entryName: String                      = "settings_long_break"
      override def cmd(time: Long): ChangeSettingsCommand = AwaitChangingLongBreak(time)
    }

    object SettingsShortBreakData extends SettingsData {
      override def entryName: String                      = "settings_short_break"
      override def cmd(time: Long): ChangeSettingsCommand = AwaitChangingShortBreak(time)
    }

    object SettingsAmountData extends SettingsData {
      override def entryName: String                      = "settings_amount"
      override def cmd(time: Long): ChangeSettingsCommand = AwaitChangingAmount(time)
    }
  }

  sealed trait StatsData extends EnumEntry

  object StatsData extends Enum[StatsData] {

    override def values: immutable.IndexedSeq[StatsData] = findValues

    object StatsLogData extends StatsData {
      override def entryName: String = "stats_log"
    }

    object StatsCountPerDayData extends StatsData {
      override def entryName: String = "stats_count_per_day"
    }

    object StatsCountPerWeekData extends StatsData {
      override def entryName: String = "stats_count_per_week"
    }

    object StatsCountPerMonthData extends StatsData {
      override def entryName: String = "stats_count_per_month"
    }
  }

  def helpMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, helpText(state), state)

  def stateMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, stateText(state), state)

  def alreadyWorkingMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, alreadyWorkingText, state)

  def alreadyBreakingMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, alreadyBreakingText, state)

  def alreadySuspendedMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, alreadySuspendedText, state)

  def cantSkipMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, cantSkipText, state)

  def workingMsg(chatId: Long, state: UserState, endTime: Long, afterPause: Boolean): TSendMessage =
    message(chatId, workingText(afterPause, state.status.remaining, endTime), state)

  def breakingMsg(chatId: Long, state: UserState, endTime: Long): TSendMessage =
    message(chatId, breakingText(state.status.remaining, endTime), state)

  def breakingAfterPauseMsg(chatId: Long, state: UserState, endTime: Long): TSendMessage =
    message(chatId, breakingAfterPauseText(state.status.remaining, endTime), state)

  def waitingWorkMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, waitingWorkText(state.status.remaining), state)

  def waitingBreakMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, waitingBreakText(state.status.remaining), state)

  def suspendedMsg(chatId: Long, state: UserState, work: Boolean): TSendMessage =
    message(chatId, suspendedText(work), state)

  def setSettingsMsg(chatId: Long, settings: UserSettings): TSendMessage =
    TSendMessage(chatId, setSettingsText(settings), setSettingsKeyboard)

  def statsMsg(chatId: Long): TSendMessage =
    TSendMessage(chatId, statsText, statsKeyboard)

  def resetMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, resetText(state), state)

  def durationMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, durationText, state)

  def longBreakMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, longBreakText, state)

  def shortBreakMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, shortBreakText, state)

  def amountMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, amountText, state)

  def settingsUpdatedMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, settingsUpdatedText, state)

  def invalidValueMsg(chatId: Long, state: UserState): TSendMessage =
    message(chatId, invalidValueText, state)

  def statsResultMsg(chatId: Long, result: UserStatsResult): TSendMessage =
    result match {
      case r: PushLog => logsMsg(chatId, r)
      case _          => TSendMessage(chatId, s"Not yet implemented, sorry, descriptor: $result")
    }

  def logsMsg(chatId: Long, result: PushLog): TSendMessage =
    TSendMessage(chatId, logsText(result.logs), logsKeyboard(result.page, result.logs.isEmpty))

  private def message(chatId: Long, text: String, state: UserState): TSendMessage =
    TSendMessage(chatId, text, keyboard(state))

  private def helpText(state: UserState): String =
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
       |${stateText(state)}
    """.stripMargin

  private def settingsText(settings: UserSettings): String =
    s"""
       |  *Tomodoro* duration: ${settings.duration} minutes
       |  *Short break* duration: ${settings.shortBreak} minutes
       |  *Long break* duration: ${settings.longBreak} minutes
       |  *Amount* of tomodoroes in cycle: ${settings.amount} tomodoroes
    """.stripMargin

  private def stateText(state: UserState): String = {
    state.status match {
      case WaitingWork(_, _) =>
        s"""
           |You are ready to start tomodoro, say *start* or *continue*
           |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case WaitingBreak(_, _) =>
        s"""
           |You are ready to taking a break, say *take a break*
           |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case Working(r, _, t) =>
        s"""
           |You are working, remaining tomodoroes in the current cycle: $r, you will finish in ${remainingMinutes(t)}
           |Your current settings: ${settingsText(state.settings)}
        """.stripMargin
      case Breaking(_, _, endTime) =>
        s"""
           |You are taking a break, you will finish in ${remainingMinutes(endTime)}
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

  private val alreadyWorkingText: String   = "You are already working, ignoring"
  private val alreadyBreakingText: String  = "You are already taking a break, ignoring"
  private val alreadySuspendedText: String = "You are already in pause, ignoring"
  private val cantSkipText: String         = "Nothing to skip, ignoring"

  private def workingText(afterPause: Boolean, remaining: Int, endTime: Long): String =
    s"""
       |${if (afterPause) "*Continuing*" else "*Starting*"}
       |Remaining tomodoroes in the current cycle: $remaining
       |Ends in ${remainingMinutes(endTime)}
    """.stripMargin

  private def breakingText(remaining: Int, endTime: Long): String =
    s"""You have a ${if (remaining == 0) "*long*" else "*short*"} break, ends in ${remainingMinutes(endTime)}"""

  private def breakingAfterPauseText(remaining: Int, endTime: Long): String =
    s"""*Continuing* a ${if (remaining == 0) "long" else "short"} break, ends in ${remainingMinutes(endTime)}"""

  private def waitingWorkText(remaining: Int): String =
    s"""*Break* finished, remaining tomodoroes in the current cycle: $remaining, say *start* or *continue* when you're ready"""

  private def waitingBreakText(remaining: Int): String =
    s"""*Tomodoro* finished, remaining  tomodoroes in the current cycle: $remaining, say *take a break* for taking a break"""

  private def suspendedText(work: Boolean): String =
    s"""${if (work) "*Tomodoro*" else "*Break*"} paused, say *continue* when you're ready"""

  private def setSettingsText(settings: UserSettings): String =
    s"""
       |Your current settings: ${settingsText(settings)}
       |*Select type* of setting you wish to update:""".stripMargin

  private val statsText: String = s"Select type of report:"

  private def resetText(state: UserState): String =
    s"""*Cycle reset*.
       |${stateText(state)} 
       |""".stripMargin

  private def logsText(logs: List[Log]): String =
    if (logs.nonEmpty)
      logs.mkString("\n")
    else "There is no activity"

  private val durationText: String        = "Say new *duration* (in minutes)"
  private val longBreakText: String       = "Say new *long break* duration (in minutes)"
  private val shortBreakText: String      = "Say new *short break* duration (in minutes)"
  private val amountText: String          = "Say new *amount* of tomodoroes in the cycle"
  private val settingsUpdatedText: String = "Settings updated"
  private val invalidValueText: String    = "Must be > 0"

  private def keyboard(state: UserState): Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(
      List(
        upButtons(state),
        middleButtons,
        downButtons
      )
    ).some

  private val setSettingsKeyboard: Option[TInlineKeyboardMarkup] =
    TInlineKeyboardMarkup(
      List(
        List(
          TInlineKeyboardButton("Duration", SettingsData.SettingsDurationData.entryName),
          TInlineKeyboardButton("Long break", SettingsData.SettingsLongBreakData.entryName)
        ),
        List(
          TInlineKeyboardButton("Short break", SettingsData.SettingsShortBreakData.entryName),
          TInlineKeyboardButton("Amount", SettingsData.SettingsAmountData.entryName)
        )
      )
    ).some

  private val statsKeyboard: Option[TInlineKeyboardMarkup] =
    TInlineKeyboardMarkup(
      List(
        List(
          TInlineKeyboardButton("Activity log", StatsData.StatsLogData.entryName),
          TInlineKeyboardButton("Last day completed", StatsData.StatsCountPerDayData.entryName)
        ),
        List(
          TInlineKeyboardButton("Last week completed", StatsData.StatsCountPerWeekData.entryName),
          TInlineKeyboardButton("Last month completed", StatsData.StatsCountPerMonthData.entryName)
        )
      )
    ).some

  private def logsKeyboard(page: Int, empty: Boolean): Option[TInlineKeyboardMarkup] =
    (page, empty) match {
      case _ if page > 0 && !empty =>
        TInlineKeyboardMarkup(
          List(
            List(
              TInlineKeyboardButton("<<", s"${StatsData.StatsLogData.entryName}:${page - 1}"),
              TInlineKeyboardButton(">>", s"${StatsData.StatsLogData.entryName}:${page + 1}")
            )
          )
        ).some
      case _ if page == 0 && !empty =>
        TInlineKeyboardMarkup(
          List(
            List(
              TInlineKeyboardButton(">>", s"${StatsData.StatsLogData.entryName}:${page + 1}")
            )
          )
        ).some
      case _ if page > 0 && empty =>
        TInlineKeyboardMarkup(
          List(
            List(
              TInlineKeyboardButton("<<", s"${StatsData.StatsLogData.entryName}:${page - 1}")
            )
          )
        ).some
      case _ => none
    }

  private def upButtons(state: UserState): List[TKeyboardButton] =
    state.status match {
      case _: FiniteStatus =>
        List(TKeyboardButton("pause"), TKeyboardButton("skip"))
      case _: SuspendedStatus =>
        List(TKeyboardButton("continue"))
      case _: WaitingBreak =>
        List(TKeyboardButton("take a break"))
      case s: WaitingWork if s.remaining == state.settings.amount =>
        List(TKeyboardButton("start"))
      case _ =>
        List(TKeyboardButton("continue"))
    }

  private val middleButtons: List[TKeyboardButton] =
    List(TKeyboardButton("reset"), TKeyboardButton("settings"), TKeyboardButton("stats"))

  private val downButtons: List[TKeyboardButton] =
    List(TKeyboardButton("state"), TKeyboardButton("help"))

  private def remainingMinutes(endTime: Long): FiniteDuration = {
    val remaining = FiniteDuration(math.max(0, endTime - OffsetDateTime.now().toEpochSecond), SECONDS).toMinutes
    Duration.create(remaining, MINUTES)
  }

  private def minutesGone(suspendTime: Long): FiniteDuration = {
    val suspended = FiniteDuration(OffsetDateTime.now().toEpochSecond - suspendTime, SECONDS).toMinutes
    Duration.create(suspended, MINUTES)
  }
}
