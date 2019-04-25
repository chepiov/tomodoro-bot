package org.chepiov.tomodoro.programs

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Repository.ActivityDescriptor._
import org.chepiov.tomodoro.algebras.Repository.ActivityLog
import org.chepiov.tomodoro.algebras.User._

/**
  * User activity log.
  */
object UserActivity {

  /**
    * Represents user state changing accordingly command.
    */
  final case class StateChangedEvent(chatId: Long, state: UserState, cmd: Command)

  def createLog(event: StateChangedEvent): Option[ActivityLog] = {
    event match {
      case tomodoroStarted(s)  => s.some
      case tomodoroFinished(s) => s.some
      case breakStarted(s)     => s.some
      case breakFinished(s)    => s.some
      case paused(s)           => s.some
      case skipped(s)          => s.some
      case settingsUpdated(s)  => s.some
      case reset(s)            => s.some
      case _                   => none
    }
  }

  private val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy hh:mm a XXX")

  private case object tomodoroStarted {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(_, Working(remaining, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = TomodoroStarted.log.format(formatter.format(time), remaining)
        ActivityLog(chatId, time, TomodoroStarted, log).some
      case _ => none
    }
  }

  private case object tomodoroFinished {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(settings, WaitingBreak(remaining, startTime), _), _: Finish)
          if remaining == settings.amount =>
        val time = at(startTime)
        val log  = CycleFinished.log.format(formatter.format(time))
        ActivityLog(chatId, time, CycleFinished, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(remaining, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = TomodoroFinished.log.format(formatter.format(time), remaining)
        ActivityLog(chatId, time, TomodoroFinished, log).some
      case _ => none
    }
  }

  private case object breakStarted {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(_, Breaking(remaining, startTime, _), _), _: Continue)
          if remaining == 0 =>
        val time = at(startTime)
        val log  = LongBreakStarted.log.format(formatter.format(time))
        ActivityLog(chatId, time, LongBreakStarted, log).some
      case StateChangedEvent(chatId, UserState(_, Breaking(_, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = ShortBreakStarted.log.format(formatter.format(time))
        ActivityLog(chatId, time, ShortBreakStarted, log).some
      case _ => none
    }
  }

  private case object breakFinished {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(settings, WaitingWork(remaining, startTime), _), _: Finish)
          if remaining == settings.amount =>
        val time = at(startTime)
        val log  = LongBreakFinished.log.format(formatter.format(time))
        ActivityLog(chatId, time, LongBreakFinished, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = ShortBreakFinished.log.format(formatter.format(time))
        ActivityLog(chatId, time, ShortBreakFinished, log).some
      case _ => none
    }
  }

  private case object paused {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(_, WorkSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = TomodoroPaused.log.format(formatter.format(time))
        ActivityLog(chatId, time, TomodoroPaused, log).some
      case StateChangedEvent(chatId, UserState(_, BreakSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = BreakPaused.log.format(formatter.format(time))
        ActivityLog(chatId, time, BreakPaused, log).some
      case _ => none
    }
  }

  private case object skipped {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = TomodoroSkipped.log.format(formatter.format(time))
        ActivityLog(chatId, time, TomodoroSkipped, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = BreakSkipped.log.format(formatter.format(time))
        ActivityLog(chatId, time, BreakSkipped, log).some
      case _ => none
    }
  }

  private case object reset {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, _, Reset(startTime)) =>
        val time = at(startTime)
        val log  = CycleReset.log.format(formatter.format(time))
        ActivityLog(chatId, time, CycleReset, log).some
      case _ => none
    }
  }

  private case object settingsUpdated {
    def unapply(event: StateChangedEvent): Option[ActivityLog] = event match {
      case StateChangedEvent(chatId, UserState(_, _, _), SetSettings(startTime)) =>
        val time = at(startTime)
        val log  = SettingsUpdated.log.format(formatter.format(time))
        ActivityLog(chatId, time, SettingsUpdated, log).some
      case _ => none
    }
  }

  private def at(epoch: Long): OffsetDateTime =
    OffsetDateTime.ofInstant(Instant.ofEpochSecond(epoch), ZoneOffset.UTC)
}
