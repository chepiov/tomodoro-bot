package org.chepiov.tomodoro.programs

import cats.data.State
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram.{apply => _, _}
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.programs.UserMessages._

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

  def query(chatId: Long, query: UserInfoQuery, state: UserState): TSendMessage =
    query match {
      case GetHelp  => helpMsg(chatId, state.settings)
      case GetState => stateMsg(chatId, state)
      case GetStats => statsMsg(chatId)
    }

  def stats(chatId: Long, result: UserStatsResult): TSendMessage =
    TSendMessage(chatId, s"Not yet implemented, sorry, descriptor: $result")

  private def advance(chatId: Long, cmd: Command, s: UserState, timeUnit: TimeUnit): (UserState, Option[TSendMessage]) =
    (s, cmd, chatId, timeUnit) match {
      case invalidTime(r @ (_, _))                   => r
      case illegalWaitingWork(r @ (_, _))            => r
      case inappropriateStateForContinue(r @ (_, _)) => r
      case inappropriateStateForFinish(r @ (_, _))   => r
      case inappropriateStateForSuspend(r @ (_, _))  => r
      case inappropriateStateForSkip(r @ (_, _))     => r
      case continueCmd(r @ (_, _))                   => r
      case finishCmd(r @ (_, _))                     => r
      case suspendCmd(r @ (_, _))                    => r
      case skipCmd(r @ (_, _))                       => r
      case changeSettingsCmd(r @ (_, _))             => r
      case resetCmd(r @ (_, _))                      => r
      case _                                         => (s, none)
    }

  private def toSeconds(duration: Int, timeUnit: TimeUnit): Long = FiniteDuration(duration.toLong, timeUnit).toSeconds

  private object invalidTime {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s @ UserState(_, status, _), cmd: Command, _, _) if cmd.time < status.startTime => (s, none).some
        case (s @ UserState(_, status: SuspendedUserStatus, _), cmd: Command, _, _) if cmd.time < status.suspend =>
          (s, none).some
        case _ => none
      }
  }

  private object illegalWaitingWork {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s @ UserState(_, WaitingWork(remaining, _), _), _, _, _) if remaining == 0 => (s, none).some
        case _                                                                           => none
      }
  }

  private object inappropriateStateForContinue {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s @ UserState(_, _: FiniteUserStatus, _), Continue(_), chatId, _) =>
          s.status match {
            case _: Working => (s, alreadyWorkingMsg(chatId)).some
            case _          => (s, alreadyBreakingMsg(chatId)).some
          }
        case _ => none
      }
  }

  private object inappropriateStateForFinish {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s @ UserState(_, _: SuspendedUserStatus, _), Finish(_), _, _) => (s, none).some
        case (s @ UserState(_, _: WaitingWork, _), Finish(_), _, _)         => (s, none).some
        case (s @ UserState(_, _: WaitingBreak, _), Finish(_), _, _)        => (s, none).some
        case _                                                              => none
      }
  }

  private object inappropriateStateForSuspend {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s @ UserState(_, _: SuspendedUserStatus, _), Suspend(_), chatId, _) =>
          (s, alreadySuspendedMsg(chatId)).some
        case (s @ UserState(_, _: WaitingWork, _), Suspend(_), chatId, _)  => (s, alreadySuspendedMsg(chatId)).some
        case (s @ UserState(_, _: WaitingBreak, _), Suspend(_), chatId, _) => (s, alreadySuspendedMsg(chatId)).some
        case _                                                             => none
      }
  }

  private object inappropriateStateForSkip {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s @ UserState(_, _: SuspendedUserStatus, _), Skip(_), chatId, _) => (s, cantSkipMsg(chatId)).some
        case (s @ UserState(_, _: WaitingWork, _), Skip(_), chatId, _)         => (s, cantSkipMsg(chatId)).some
        case (s @ UserState(_, _: WaitingBreak, _), Skip(_), chatId, _)        => (s, cantSkipMsg(chatId)).some
        case _                                                                 => none
      }
  }

  private object continueCmd {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s, cmd @ Continue(_), chatId, timeUnit) => continue(chatId, cmd, s, timeUnit).some
        case _                                        => none
      }
  }

  private object finishCmd {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s, cmd @ Finish(_), chatId, _) => finish(chatId, cmd, s).some
        case _                               => none
      }
  }

  private object suspendCmd {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s, cmd @ Suspend(_), chatId, _) => suspend(chatId, cmd, s).some
        case _                                => none
      }
  }

  private object skipCmd {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s, Skip(t), chatId, _) => finish(chatId, Finish(t), s).some
        case _                       => none
      }
  }

  private object changeSettingsCmd {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (s, cmd @ SetSettings(_), chatId, _)       => changeSettings(chatId, cmd, s).some
        case (s, cmd: ChangeSettingsCommand, chatId, _) => changeSettings(chatId, cmd, s).some
        case _                                          => none
      }
  }

  private object resetCmd {
    def unapply(uc: (UserState, Command, Long, TimeUnit)): Option[(UserState, Option[TSendMessage])] =
      uc match {
        case (us @ UserState(settings @ UserSettings(_, _, _, a), _, _), Reset(t), chatId, _) =>
          val remaining = a
          val start     = t
          (us.copy(status = WaitingWork(remaining, start)), resetMsg(chatId, settings)).some
        case _ => none
      }
  }

  private def continue(
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
        (us.copy(status = Working(remaining, start, end)), workingMsg(chatId, remaining, end, afterPause = false))
      case (us @ UserState(UserSettings(_, sb, lb, _), WaitingBreak(r, _), _), Continue(t)) =>
        val remaining = r
        val start     = t
        val end       = if (r == 0) start + toSeconds(lb, timeUnit) else start + toSeconds(sb, timeUnit)
        (us.copy(status = Breaking(remaining, start, end)), breakingMsg(chatId, remaining, end))
      case (us @ UserState(UserSettings(d, _, _, _), WorkSuspended(r, startedAt, suspendAt), _), Continue(t)) =>
        val remaining = r
        val start     = t
        val worked    = suspendAt - startedAt
        val end       = start + toSeconds(d, timeUnit) - worked
        (us.copy(status = Working(remaining, start, end)), workingMsg(chatId, remaining, end, afterPause = true))
      case (us @ UserState(UserSettings(_, sb, lb, _), BreakSuspended(r, startedAt, suspendAt), _), Continue(t)) =>
        val remaining = r
        val start     = t
        val suspend   = suspendAt - startedAt
        val end =
          if (remaining == 0) start + toSeconds(lb, timeUnit) - suspend else start + toSeconds(sb, timeUnit) - suspend
        (us.copy(status = Breaking(remaining, start, end)), breakingAfterPauseMsg(chatId, remaining, end))
      case _ => (s, none)
    }

  private def finish(
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
        (us.copy(status = WaitingWork(remaining, start)), waitingWorkMsg(chatId, remaining))
      case _ => (s, none)
    }

  private def suspend(
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

  private def changeSettings(
      chatId: Long,
      cmd: Command,
      s: UserState
  ): (UserState, Option[TSendMessage]) =
    (s, cmd) match {
      case (us, SetSettings(_))                                   => (us, setSettingsMsg(chatId, us.settings))
      case (us, AwaitChangingDuration(t))                         => (us.copy(settingsUpdate = DurationUpdate(t)), durationMsg(chatId))
      case (us, AwaitChangingLongBreak(t))                        => (us.copy(settingsUpdate = LongBreakUpdate(t)), longBreakMsg(chatId))
      case (us, AwaitChangingShortBreak(t))                       => (us.copy(settingsUpdate = ShortBreakUpdate(t)), shortBreakMsg(chatId))
      case (us, AwaitChangingAmount(t))                           => (us.copy(settingsUpdate = AmountUpdate(t)), amountMsg(chatId))
      case (us @ UserState(_, _, NotUpdate), _: SetSettingsValue) => (us, none)
      case (us, SetSettingsValue(_, value)) if value <= 0         => (us, invalidValueMsg(chatId))
      case (us @ UserState(settings, _, settingsUpdate), SetSettingsValue(_, value)) if value > 0 =>
        val updatedSettings = settingsUpdate match {
          case _: DurationUpdate   => settings.copy(duration = value)
          case _: ShortBreakUpdate => settings.copy(shortBreak = value)
          case _: LongBreakUpdate  => settings.copy(longBreak = value)
          case _: AmountUpdate     => settings.copy(amount = value)
          case _                   => settings
        }
        (us.copy(settings = updatedSettings, settingsUpdate = NotUpdate), settingsUpdatedMsg(chatId))
      case _ => (s, none)
    }
}
