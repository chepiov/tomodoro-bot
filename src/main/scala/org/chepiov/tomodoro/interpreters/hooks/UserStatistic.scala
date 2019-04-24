package org.chepiov.tomodoro.interpreters.hooks

import java.time.format.DateTimeFormatter
import java.time.{Instant, OffsetDateTime, ZoneOffset}

import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.option._
import cats.{Applicative, ApplicativeError}
import enumeratum.EnumEntry.Uppercase
import enumeratum._
import org.chepiov.tomodoro.actors.UserActor.StateChangedEvent
import org.chepiov.tomodoro.algebras.Repository
import org.chepiov.tomodoro.algebras.User._

import scala.collection.immutable
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

  private def saveLog(log: Log): F[Try[Unit]] = {
    val result = repository.addLog(log)
    result *> Applicative[F].pure(Try(())).handleErrorWith { e =>
      Applicative[F].pure(Failure(e))
    }
  }
}

case object UserStatistic {
  import org.chepiov.tomodoro.interpreters.hooks.StatDescriptor._

  private val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy hh:mm a XXX")

  case object tomodoroStarted {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(_, Working(remaining, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = TomodoroStarted.log.format(formatter.format(time), remaining)
        Log(chatId, time, TomodoroStarted, log).some
      case _ => none
    }
  }

  case object tomodoroFinished {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(settings, WaitingBreak(remaining, startTime), _), _: Finish)
          if remaining == settings.amount =>
        val time = at(startTime)
        val log  = CycleFinished.log.format(formatter.format(time))
        Log(chatId, time, CycleFinished, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(remaining, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = TomodoroFinished.log.format(formatter.format(time), remaining)
        Log(chatId, time, TomodoroFinished, log).some
      case _ => none
    }
  }

  case object breakStarted {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(_, Breaking(remaining, startTime, _), _), _: Continue)
          if remaining == 0 =>
        val time = at(startTime)
        val log  = LongBreakStarted.log.format(formatter.format(time))
        Log(chatId, time, LongBreakStarted, log).some
      case StateChangedEvent(chatId, UserState(_, Breaking(_, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = ShortBreakStarted.log.format(formatter.format(time))
        Log(chatId, time, ShortBreakStarted, log).some
      case _ => none
    }
  }

  case object breakFinished {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(_, WaitingWork(remaining, startTime), _), _: Finish) if remaining == 0 =>
        val time = at(startTime)
        val log  = ShortBreakFinished.log.format(formatter.format(time))
        Log(chatId, time, ShortBreakFinished, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = LongBreakFinished.log.format(formatter.format(time))
        Log(chatId, time, LongBreakFinished, log).some
      case _ => none
    }
  }

  case object paused {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(_, WorkSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = TomodoroPaused.log.format(formatter.format(time))
        Log(chatId, time, TomodoroPaused, log).some
      case StateChangedEvent(chatId, UserState(_, BreakSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = BreakPaused.log.format(formatter.format(time))
        Log(chatId, time, BreakPaused, log).some
      case _ => none
    }
  }

  case object skipped {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = TomodoroSkipped.log.format(formatter.format(time))
        Log(chatId, time, TomodoroSkipped, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = BreakSkipped.log.format(formatter.format(time))
        Log(chatId, time, BreakSkipped, log).some
      case _ => none
    }
  }

  case object reset {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, _, Reset(startTime)) =>
        val time = at(startTime)
        val log  = CycleReset.log.format(formatter.format(time))
        Log(chatId, time, CycleReset, log).some
      case _ => none
    }
  }

  case object settingsUpdated {
    def unapply(event: StateChangedEvent): Option[Log] = event match {
      case StateChangedEvent(chatId, UserState(_, _, _), SetSettings(startTime)) =>
        val time = at(startTime)
        val log  = SettingsUpdated.log.format(formatter.format(time))
        Log(chatId, time, SettingsUpdated, log).some
      case _ => none
    }
  }

  private def at(epoch: Long): OffsetDateTime =
    OffsetDateTime.ofInstant(Instant.ofEpochSecond(epoch), ZoneOffset.UTC)
}

sealed trait StatDescriptor extends EnumEntry with Uppercase {
  def log: String
}

object StatDescriptor extends Enum[StatDescriptor] {
  override def values: immutable.IndexedSeq[StatDescriptor] = findValues

  case object CycleFinished extends StatDescriptor {
    override def log: String = "Cycle finished at: %s"
  }

  case object TomodoroFinished extends StatDescriptor {
    override def log: String = "Tomodoro finished at: %s, remaining tomodoroes: %d"
  }

  case object ShortBreakFinished extends StatDescriptor {
    override def log: String = "Short break finished at: %s"
  }

  case object LongBreakFinished extends StatDescriptor {
    override def log: String = "Long break finished at: %s"
  }

  case object TomodoroPaused extends StatDescriptor {
    override def log: String = "Tomodoro paused at: %s"
  }

  case object BreakPaused extends StatDescriptor {
    override def log: String = "Break paused at: %s"
  }

  case object TomodoroStarted extends StatDescriptor {
    override def log: String = "Tomodoro started or continued at: %s, remaining tomodoroes: %d"
  }

  case object ShortBreakStarted extends StatDescriptor {
    override def log: String = "Short break started or continued at: %s"
  }

  case object LongBreakStarted extends StatDescriptor {
    override def log: String = "Long break started or continued at: %s"
  }

  case object TomodoroSkipped extends StatDescriptor {
    override def log: String = "Tomodoro skipped at: %s"
  }

  case object BreakSkipped extends StatDescriptor {
    override def log: String = "Break skipped at: %s"
  }

  case object SettingsUpdated extends StatDescriptor {
    override def log: String = "Settings updated at: %s"
  }

  case object CycleReset extends StatDescriptor {
    override def log: String = "Cycle reset at: %s"
  }
}
