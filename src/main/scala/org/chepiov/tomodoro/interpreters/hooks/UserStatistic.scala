package org.chepiov.tomodoro.interpreters.hooks

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.option._
import cats.{Applicative, ApplicativeError}
import org.chepiov.tomodoro.actors.UserActor.StateChangedEvent
import org.chepiov.tomodoro.algebras.Repository
import org.chepiov.tomodoro.algebras.Repository.StatLog
import org.chepiov.tomodoro.algebras.User._

import scala.util.{Failure, Success, Try}

class UserStatistic[F[_]: ApplicativeError[?[_], Throwable]](repository: Repository[F]) {
  import UserStatistic._

  def consume(event: StateChangedEvent): F[Try[Unit]] = {
    event match {
      case tomodoroStarted(s)  => saveLog(s)
      case tomodoroFinished(s) => saveLog(s)
      case breakStarted(s)     => saveLog(s)
      case breakFinished(s)    => saveLog(s)
      case paused(s)           => saveLog(s)
      case skipped(s)          => saveLog(s)
      case settingsUpdated(s)  => saveLog(s)
      case reset(s)            => saveLog(s)
      case _                   => Applicative[F].pure(Success(()))
    }
  }

  private def saveLog(log: StatLog): F[Try[Unit]] = {
    val result = repository.addLog(log)
    result *> Applicative[F].pure(Try(())).handleErrorWith { e =>
      Applicative[F].pure(Failure(e))
    }
  }
}

case object UserStatistic {
  import org.chepiov.tomodoro.algebras.Repository.StatDescriptor._

  private val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy hh:mm a XXX")

  case object tomodoroStarted {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(_, Working(remaining, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = TomodoroStarted.log.format(formatter.format(time), remaining)
        StatLog(chatId, time, TomodoroStarted, log).some
      case _ => none
    }
  }

  case object tomodoroFinished {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(settings, WaitingBreak(remaining, startTime), _), _: Finish)
          if remaining == settings.amount =>
        val time = at(startTime)
        val log  = CycleFinished.log.format(formatter.format(time))
        StatLog(chatId, time, CycleFinished, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(remaining, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = TomodoroFinished.log.format(formatter.format(time), remaining)
        StatLog(chatId, time, TomodoroFinished, log).some
      case _ => none
    }
  }

  case object breakStarted {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(_, Breaking(remaining, startTime, _), _), _: Continue)
          if remaining == 0 =>
        val time = at(startTime)
        val log  = LongBreakStarted.log.format(formatter.format(time))
        StatLog(chatId, time, LongBreakStarted, log).some
      case StateChangedEvent(chatId, UserState(_, Breaking(_, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = ShortBreakStarted.log.format(formatter.format(time))
        StatLog(chatId, time, ShortBreakStarted, log).some
      case _ => none
    }
  }

  case object breakFinished {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(settings, WaitingWork(remaining, startTime), _), _: Finish)
          if remaining == settings.amount =>
        val time = at(startTime)
        val log  = LongBreakFinished.log.format(formatter.format(time))
        StatLog(chatId, time, LongBreakFinished, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = ShortBreakFinished.log.format(formatter.format(time))
        StatLog(chatId, time, ShortBreakFinished, log).some
      case _ => none
    }
  }

  case object paused {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(_, WorkSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = TomodoroPaused.log.format(formatter.format(time))
        StatLog(chatId, time, TomodoroPaused, log).some
      case StateChangedEvent(chatId, UserState(_, BreakSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = BreakPaused.log.format(formatter.format(time))
        StatLog(chatId, time, BreakPaused, log).some
      case _ => none
    }
  }

  case object skipped {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = TomodoroSkipped.log.format(formatter.format(time))
        StatLog(chatId, time, TomodoroSkipped, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = BreakSkipped.log.format(formatter.format(time))
        StatLog(chatId, time, BreakSkipped, log).some
      case _ => none
    }
  }

  case object reset {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, _, Reset(startTime)) =>
        val time = at(startTime)
        val log  = CycleReset.log.format(formatter.format(time))
        StatLog(chatId, time, CycleReset, log).some
      case _ => none
    }
  }

  case object settingsUpdated {
    def unapply(event: StateChangedEvent): Option[StatLog] = event match {
      case StateChangedEvent(chatId, UserState(_, _, _), SetSettings(startTime)) =>
        val time = at(startTime)
        val log  = SettingsUpdated.log.format(formatter.format(time))
        StatLog(chatId, time, SettingsUpdated, log).some
      case _ => none
    }
  }

  private def at(epoch: Long): OffsetDateTime =
    OffsetDateTime.ofInstant(Instant.ofEpochSecond(epoch), ZoneOffset.UTC)
}
