package org.chepiov.tomodoro.programs

import cats.data.State
import org.chepiov.tomodoro.algebras.User._

import scala.concurrent.duration.{FiniteDuration, TimeUnit}

/**
  * User state program.
  */
case object UserStateMachine {

  def advance(cmd: Command, timeUnit: TimeUnit): State[UserState, Answer] =
    for {
      init            <- State.get[UserState]
      (state, answer) = perform(cmd, init, timeUnit)
      _               <- State.set(state)
    } yield answer

  private def perform(cmd: Command, s: UserState, timeUnit: TimeUnit): (UserState, Answer) =
    (s, cmd) match {
      case invalidTime()                   => (s, InvalidTime)
      case illegalWaitingWork()            => (s, IllegalState)
      case inappropriateStateForContinue() => (s, AlreadyInProgress)
      case inappropriateStateForFinish()   => (s, NotYetInProgress)
      case inappropriateStateForSuspend()  => (s, NotYetInProgress)
      case (us @ UserState(UserSettings(d, _, _, _), WaitingWork(r, _)), Continue(t)) =>
        val remaining = r - 1
        val start     = t
        val end       = start + toSeconds(d, timeUnit)
        (us.copy(status = Working(remaining, start, end)), Ok)
      case (us @ UserState(UserSettings(_, sb, lb, _), WaitingBreak(r, _)), Continue(t)) =>
        val remaining = r
        val start     = t
        val end       = if (r == 0) start + toSeconds(lb, timeUnit) else start + toSeconds(sb, timeUnit)
        (us.copy(status = Breaking(remaining, start, end)), Ok)
      case (us @ UserState(UserSettings(d, _, _, _), WorkSuspended(r, startedAt, suspendAt)), Continue(t)) =>
        val remaining = r
        val start     = t
        val worked    = suspendAt - startedAt
        val end       = start + toSeconds(d, timeUnit) - worked
        (us.copy(status = Working(remaining, start, end)), Ok)
      case (us @ UserState(UserSettings(_, sb, lb, _), BreakSuspended(r, startedAt, suspendAt)), Continue(t)) =>
        val remaining = r
        val start     = t
        val suspend   = suspendAt - startedAt
        val end =
          if (remaining == 0) start + toSeconds(lb, timeUnit) - suspend else start + toSeconds(sb, timeUnit) - suspend
        (us.copy(status = Breaking(remaining, start, end)), Ok)
      case (us @ UserState(UserSettings(_, _, _, _), Working(r, _, _)), Finish(t)) =>
        val remaining = r
        val start     = t
        (us.copy(status = WaitingBreak(remaining, start)), Ok)
      case (us @ UserState(UserSettings(_, _, _, a), Breaking(r, _, _)), Finish(t)) =>
        val remaining = if (r == 0) a else r
        val start     = t
        (us.copy(status = WaitingWork(remaining, start)), Ok)
      case (us @ UserState(UserSettings(_, _, _, _), Working(r, startedAt, _)), Suspend(t)) =>
        val remaining = r
        val start     = startedAt
        val suspend   = t
        (us.copy(status = WorkSuspended(remaining, start, suspend)), Ok)
      case (us @ UserState(UserSettings(_, _, _, _), Breaking(r, startedAt, _)), Suspend(t)) =>
        val remaining = r
        val start     = startedAt
        val suspend   = t
        (us.copy(status = BreakSuspended(remaining, start, suspend)), Ok)
      case (us, SetSettings(_, settings)) =>
        (us.copy(settings = settings), Ok)
      case (us @ UserState(UserSettings(_, _, _, a), _), Stop(t)) =>
        val remaining = a
        val start     = t
        (us.copy(status = WaitingWork(remaining, start)), Ok)
      case _ => (s, IllegalState)
    }

  private def toSeconds(duration: Int, timeUnit: TimeUnit): Long = FiniteDuration(duration.toLong, timeUnit).toSeconds

  private object invalidTime {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, s), c: Command) if c.time < s.startTime                        => true
      case (UserState(_, s: SuspendedUserStatus), c: Command) if c.time < s.suspend => true
      case _                                                                        => false
    }
  }

  private object illegalWaitingWork {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, WaitingWork(remaining, _)), _) if remaining == 0 => true
      case _                                                              => false
    }
  }

  private object inappropriateStateForContinue {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, _: FiniteUserStatus), Continue(_)) => true
      case _                                                => false
    }
  }

  private object inappropriateStateForFinish {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, _: SuspendedUserStatus), Finish(_)) => true
      case (UserState(_, _: WaitingWork), Finish(_))         => true
      case (UserState(_, _: WaitingBreak), Finish(_))        => true
      case _                                                 => false
    }
  }

  private object inappropriateStateForSuspend {
    def unapply(uc: (UserState, Command)): Boolean = uc match {
      case (UserState(_, _: SuspendedUserStatus), Suspend(_)) => true
      case (UserState(_, _: WaitingWork), Suspend(_))         => true
      case (UserState(_, _: WaitingBreak), Suspend(_))        => true
      case _                                                  => false
    }
  }
}
