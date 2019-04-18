package org.chepiov.tomodoro.programs

import cats.data.State
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram.{apply => _, _}
import org.chepiov.tomodoro.algebras.User._

import scala.concurrent.duration.{FiniteDuration, TimeUnit}

/**
  * User state machine.
  */
case object UserStateMachine {

  def advance(chatId: Long, cmd: Command, timeUnit: TimeUnit): State[UserState, Option[TSendMessage]] =
    for {
      init            <- State.get[UserState]
      (state, answer) = advance(chatId, cmd, init, timeUnit)
      _               <- State.set(state)
    } yield answer

  private def advance(chatId: Long, cmd: Command, s: UserState, timeUnit: TimeUnit): (UserState, Option[TSendMessage]) =
    (s, cmd) match {
      case invalidTime()        => (s, none)
      case illegalWaitingWork() => (s, none)
      case inappropriateStateForContinue() =>
        s.status match {
          case _: Working => (s, alreadyWorkingMsg(chatId))
          case _          => (s, alreadyBreakingMsg(chatId))
        }
      case inappropriateStateForFinish()  => (s, none)
      case inappropriateStateForSuspend() => (s, alreadySuspendedMsg(chatId))
      case continueCmd()                  => continueAdvance(chatId, cmd, s, timeUnit)
      case finishCmd()                    => finishAdvance(chatId, cmd, s)
      case suspendCmd()                   => suspendAdvance(chatId, cmd, s)
      case changeSettingsCmd()            => changeSettingsAdvance(chatId, cmd, s)
      case (us @ UserState(settings @ UserSettings(_, _, _, a), _, _), Reset(t)) =>
        val remaining = a
        val start     = t
        (us.copy(status = WaitingWork(remaining, start)), resetMsg(chatId, settings))
      case _ => (s, none)
    }

  private def continueAdvance(
      chatId: Long,
      cmd: Command,
      s: UserState,
      timeUnit: TimeUnit
  ): (UserState, Option[TSendMessage]) =
    (s, cmd) match {
      case (us @ UserState(UserSettings(d, _, _, _), WaitingWork(r, _), _), Continue(t)) =>
        val remaining = r - 1
        val start     = t
        val end       = start + toSeconds(d, timeUnit)
        (us.copy(status = Working(remaining, start, end)), workingMsg(chatId, remaining, afterPause = false))
      case (us @ UserState(UserSettings(_, sb, lb, _), WaitingBreak(r, _), _), Continue(t)) =>
        val remaining = r
        val start     = t
        val end       = if (r == 0) start + toSeconds(lb, timeUnit) else start + toSeconds(sb, timeUnit)
        (us.copy(status = Breaking(remaining, start, end)), breakingMsg(chatId, remaining))
      case (us @ UserState(UserSettings(d, _, _, _), WorkSuspended(r, startedAt, suspendAt), _), Continue(t)) =>
        val remaining = r
        val start     = t
        val worked    = suspendAt - startedAt
        val end       = start + toSeconds(d, timeUnit) - worked
        (us.copy(status = Working(remaining, start, end)), workingMsg(chatId, remaining, afterPause = true))
      case (us @ UserState(UserSettings(_, sb, lb, _), BreakSuspended(r, startedAt, suspendAt), _), Continue(t)) =>
        val remaining = r
        val start     = t
        val suspend   = suspendAt - startedAt
        val end =
          if (remaining == 0) start + toSeconds(lb, timeUnit) - suspend else start + toSeconds(sb, timeUnit) - suspend
        (us.copy(status = Breaking(remaining, start, end)), breakingAfterPauseMsg(chatId, remaining))
      case _ => (s, none)
    }

  private def finishAdvance(
      chatId: Long,
      cmd: Command,
      s: UserState
  ): (UserState, Option[TSendMessage]) =
    (s, cmd) match {
      case (us @ UserState(UserSettings(_, _, _, _), Working(r, _, _), _), Finish(t)) =>
        val remaining = r
        val start     = t
        (us.copy(status = WaitingBreak(remaining, start)), waitingBreakMsg(chatId, remaining))
      case (us @ UserState(UserSettings(_, _, _, a), Breaking(r, _, _), _), Finish(t)) =>
        val remaining = if (r == 0) a else r
        val start     = t
        (us.copy(status = WaitingWork(remaining, start)), waitingWorkMsg(chatId, remaining, remaining == a))
      case _ => (s, none)
    }

  private def suspendAdvance(
      chatId: Long,
      cmd: Command,
      s: UserState
  ): (UserState, Option[TSendMessage]) =
    (s, cmd) match {
      case (us @ UserState(UserSettings(_, _, _, _), Working(r, startedAt, _), _), Suspend(t)) =>
        val remaining = r
        val start     = startedAt
        val suspend   = t
        (us.copy(status = WorkSuspended(remaining, start, suspend)), suspendedMsg(chatId, work = true))
      case (us @ UserState(UserSettings(_, _, _, _), Breaking(r, startedAt, _), _), Suspend(t)) =>
        val remaining = r
        val start     = startedAt
        val suspend   = t
        (us.copy(status = BreakSuspended(remaining, start, suspend)), suspendedMsg(chatId, work = false))
      case _ => (s, none)
    }

  private def changeSettingsAdvance(
      chatId: Long,
      cmd: Command,
      s: UserState
  ): (UserState, Option[TSendMessage]) =
    (s, cmd) match {
      case (us, SetSettings(_)) =>
        (us, setSettingsMsg(chatId))
      case (us, AwaitChangingDuration(t)) =>
        (
          us.copy(settingsUpdate = DurationUpdate(t)),
          TSendMessage(chatId, "Input new duration (in minutes)", none).some
        )
      case (us, AwaitChangingLongBreak(t)) =>
        (
          us.copy(settingsUpdate = LongBreakUpdate(t)),
          TSendMessage(chatId, "Input new long break duration (in minutes)", none).some
        )
      case (us, AwaitChangingShortBreak(t)) =>
        (
          us.copy(settingsUpdate = ShortBreakUpdate(t)),
          TSendMessage(chatId, "Input new short break duration (in minutes)", none).some
        )
      case (us, AwaitChangingAmount(t)) =>
        (
          us.copy(settingsUpdate = AmountUpdate(t)),
          TSendMessage(chatId, "Input new amount of tomodoroes in the cycle", none).some
        )
      case (us @ UserState(_, _, NotUpdate), _: SetSettingsValue) =>
        (us, none)
      case (us @ UserState(settings, _, settingsUpdate), SetSettingsValue(_, value)) if value > 0 =>
        val updatedSettings = settingsUpdate match {
          case _: DurationUpdate   => settings.copy(duration = value)
          case _: ShortBreakUpdate => settings.copy(shortBreak = value)
          case _: LongBreakUpdate  => settings.copy(longBreak = value)
          case _: AmountUpdate     => settings.copy(amount = value)
          case _                   => settings
        }
        (us.copy(settings = updatedSettings), TSendMessage(chatId, "Settings updated").some)
      case (us, SetSettingsValue(_, _)) =>
        (us, TSendMessage(chatId, "Value must be > 0").some)
      case _ => (s, none)
    }

  private def toSeconds(duration: Int, timeUnit: TimeUnit): Long = FiniteDuration(duration.toLong, timeUnit).toSeconds

  private object invalidTime {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, s, _), c: Command) if c.time < s.startTime                    => true
      case (UserState(_, s: SuspendedUserStatus, _), c: Command) if c.time < s.suspend => true
      case _                                                                           => false
    }
  }

  private object illegalWaitingWork {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, WaitingWork(remaining, _), _), _) if remaining == 0 => true
      case _                                                                 => false
    }
  }

  private object inappropriateStateForContinue {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, _: FiniteUserStatus, _), Continue(_)) => true
      case _                                                   => false
    }
  }

  private object inappropriateStateForFinish {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, _: SuspendedUserStatus, _), Finish(_)) => true
      case (UserState(_, _: WaitingWork, _), Finish(_))         => true
      case (UserState(_, _: WaitingBreak, _), Finish(_))        => true
      case _                                                    => false
    }
  }

  private object inappropriateStateForSuspend {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, _: SuspendedUserStatus, _), Suspend(_)) => true
      case (UserState(_, _: WaitingWork, _), Suspend(_))         => true
      case (UserState(_, _: WaitingBreak, _), Suspend(_))        => true
      case _                                                     => false
    }
  }

  private object continueCmd {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (_, Continue(_)) => true
      case _                => false
    }
  }

  private object finishCmd {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (_, Finish(_)) => true
      case _              => false
    }
  }

  private object suspendCmd {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (_, Suspend(_)) => true
      case _               => false
    }
  }

  private object changeSettingsCmd {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (_, SetSettings(_))           => true
      case (_, _: ChangeSettingsCommand) => true
      case _                             => false
    }
  }

  private def alreadyWorkingMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already working", none).some
  private def alreadyBreakingMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already taking rest", none).some
  private def alreadySuspendedMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, "You are already in pause", none).some

  private def workingMsg(chatId: Long, remaining: Int, afterPause: Boolean): Option[TSendMessage] =
    TSendMessage(
      chatId,
      s"""Tomodoro ${if (afterPause) "continued" else "started"}, remaining: $remaining""",
      workingKeyboard
    ).some

  private def breakingMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(chatId, s"""You have a ${if (remaining == 0) "long" else "short"} break""", breakingKeyboard).some

  private def breakingAfterPauseMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(chatId, s"""Continuing a ${if (remaining == 0) "long" else "short"} break""", breakingKeyboard).some

  private def waitingWorkMsg(chatId: Long, remaining: Int, cycleStart: Boolean): Option[TSendMessage] =
    TSendMessage(chatId, s"""Break finished, remaining: $remaining""", waitingKeyboard(cycleStart)).some

  private def waitingBreakMsg(chatId: Long, remaining: Int): Option[TSendMessage] =
    TSendMessage(chatId, s"""Tomodoro finished, remaining: $remaining""", waitingKeyboard(false)).some

  private def suspendedMsg(chatId: Long, work: Boolean): Option[TSendMessage] =
    TSendMessage(chatId, s"""${if (work) "Tomodoro" else "Break"} paused""", waitingKeyboard(false)).some

  private def setSettingsMsg(chatId: Long): Option[TSendMessage] =
    TSendMessage(chatId, s"Choose type of setting:", setSettingsKeyboard).some

  private def resetMsg(chatId: Long, settings: UserSettings): Option[TSendMessage] =
    TSendMessage(chatId, s"""Tomodoro reset, Your settings: ${settingsText(settings)}""", waitingKeyboard(true)).some

  private val downButtons: List[TKeyboardButton] =
    List(TKeyboardButton("/settings"), TKeyboardButton("/stats"), TKeyboardButton("/help"), TKeyboardButton("/reset"))

  private val workingKeyboard: Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(List(List(TKeyboardButton("/pause")), downButtons)).some

  private val breakingKeyboard: Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(List(List(TKeyboardButton("/pause"), TKeyboardButton("/skip")), downButtons)).some

  val SettingDurationData   = "settings_duration"
  val SettingLongBreakData  = "settings_long_break"
  val SettingShortBreakData = "settings_short_break"
  val SettingAmountData     = "settings_amount"

  private val setSettingsKeyboard: Option[TInlineKeyboardMarkup] =
    TInlineKeyboardMarkup(
      List(
        List(
          TInlineKeyboardButton("Duration", SettingDurationData),
          TInlineKeyboardButton("Long break", SettingLongBreakData),
          TInlineKeyboardButton("Short break", SettingShortBreakData),
          TInlineKeyboardButton("Amount", SettingAmountData)
        )
      )
    ).some

  private def waitingKeyboard(cycleStart: Boolean): Option[TReplyKeyboardMarkup] =
    TReplyKeyboardMarkup(List(List(TKeyboardButton(s"""${if (cycleStart) "/start" else "/continue"}""")), downButtons)).some

  private def settingsText(settings: UserSettings): String =
    s"""
       |
       | Tomodoro duration: ${settings.duration}.
       | Short break duration: ${settings.shortBreak}.
       | Long break duration: ${settings.longBreak}.
       | Amount of tomodoroes in cycle: ${settings.amount}.
       |""".stripMargin
}
