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

  private def saveLog(stat: StatDescriptor): F[Try[Unit]] = {
    val result = repository.addLog(stat.chatId, stat.time, stat.descriptor, stat.log)
    result *> Applicative[F].pure(Try(())).handleErrorWith { e =>
      Applicative[F].pure(Failure(e))
    }
  }
}

case object UserStatistic {
  import org.chepiov.tomodoro.interpreters.hooks.StatType._

  private val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy hh:mm a XXX")

  final private[hooks] case class StatDescriptor(
      chatId: Long,
      time: OffsetDateTime,
      descriptor: String,
      log: String
  )

  case object tomodoroStarted {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(_, Working(remaining, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = TomodoroStarted.log.format(formatter.format(time), remaining)
        StatDescriptor(chatId, time, TomodoroStarted.entryName, log).some
      case _ => none
    }
  }

  case object tomodoroFinished {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(settings, WaitingBreak(remaining, startTime), _), _: Finish)
          if remaining == settings.amount =>
        val time = at(startTime)
        val log  = CycleFinished.log.format(formatter.format(time))
        StatDescriptor(chatId, time, CycleFinished.entryName, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(remaining, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = TomodoroFinished.log.format(formatter.format(time), remaining)
        StatDescriptor(chatId, time, TomodoroFinished.entryName, log).some
      case _ => none
    }
  }

  case object breakStarted {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(_, Breaking(remaining, startTime, _), _), _: Continue)
          if remaining == 0 =>
        val time = at(startTime)
        val log  = LongBreakStarted.log.format(formatter.format(time))
        StatDescriptor(chatId, time, LongBreakStarted.entryName, log).some
      case StateChangedEvent(chatId, UserState(_, Breaking(_, startTime, _), _), _: Continue) =>
        val time = at(startTime)
        val log  = ShortBreakStarted.log.format(formatter.format(time))
        StatDescriptor(chatId, time, ShortBreakStarted.entryName, log).some
      case _ => none
    }
  }

  case object breakFinished {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(_, WaitingWork(remaining, startTime), _), _: Finish) if remaining == 0 =>
        val time = at(startTime)
        val log  = ShortBreakFinished.log.format(formatter.format(time))
        StatDescriptor(chatId, time, ShortBreakFinished.entryName, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Finish) =>
        val time = at(startTime)
        val log  = LongBreakFinished.log.format(formatter.format(time))
        StatDescriptor(chatId, time, LongBreakFinished.entryName, log).some
      case _ => none
    }
  }

  case object paused {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(_, WorkSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = TomodoroPaused.log.format(formatter.format(time))
        StatDescriptor(chatId, time, TomodoroPaused.entryName, log).some
      case StateChangedEvent(chatId, UserState(_, BreakSuspended(_, _, suspendTime), _), _: Suspend) =>
        val time = at(suspendTime)
        val log  = BreakPaused.log.format(formatter.format(time))
        StatDescriptor(chatId, time, BreakPaused.entryName, log).some
      case _ => none
    }
  }

  case object skipped {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(_, WaitingBreak(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = TomodoroSkipped.log.format(formatter.format(time))
        StatDescriptor(chatId, time, TomodoroSkipped.entryName, log).some
      case StateChangedEvent(chatId, UserState(_, WaitingWork(_, startTime), _), _: Skip) =>
        val time = at(startTime)
        val log  = BreakSkipped.log.format(formatter.format(time))
        StatDescriptor(chatId, time, BreakSkipped.entryName, log).some
      case _ => none
    }
  }

  case object reset {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, _, Reset(startTime)) =>
        val time = at(startTime)
        val log  = CycleReset.log.format(formatter.format(time))
        StatDescriptor(chatId, time, CycleReset.entryName, log).some
      case _ => none
    }
  }

  case object settingsUpdated {
    def unapply(event: StateChangedEvent): Option[StatDescriptor] = event match {
      case StateChangedEvent(chatId, UserState(_, _, _), SetSettings(startTime)) =>
        val time = at(startTime)
        val log  = SettingsUpdated.log.format(formatter.format(time))
        StatDescriptor(chatId, time, SettingsUpdated.entryName, log).some
      case _ => none
    }
  }

  private def at(epoch: Long): OffsetDateTime =
    OffsetDateTime.ofInstant(Instant.ofEpochSecond(epoch), ZoneOffset.UTC)
}

sealed trait StatType extends EnumEntry with Uppercase {
  def log: String
}

object StatType extends Enum[StatType] {
  override def values: immutable.IndexedSeq[StatType] = findValues

  case object CycleFinished extends StatType {
    override def log: String = "Cycle finished at: %s"
  }

  case object TomodoroFinished extends StatType {
    override def log: String = "Tomodoro finished at: %s, remaining tomodoroes: %d"
  }

  case object ShortBreakFinished extends StatType {
    override def log: String = "Short break finished at: %s"
  }

  case object LongBreakFinished extends StatType {
    override def log: String = "Long break finished at: %s"
  }

  case object TomodoroPaused extends StatType {
    override def log: String = "Tomodoro paused at: %s"
  }

  case object BreakPaused extends StatType {
    override def log: String = "Break paused at: %s"
  }

  case object TomodoroStarted extends StatType {
    override def log: String = "Tomodoro started or continued at: %s, remaining tomodoroes: %d"
  }

  case object ShortBreakStarted extends StatType {
    override def log: String = "Short break started or continued at: %s"
  }

  case object LongBreakStarted extends StatType {
    override def log: String = "Long break started or continued at: %s"
  }

  case object TomodoroSkipped extends StatType {
    override def log: String = "Tomodoro skipped at: %s"
  }

  case object BreakSkipped extends StatType {
    override def log: String = "Break skipped at: %s"
  }

  case object SettingsUpdated extends StatType {
    override def log: String = "Settings updated at: %s"
  }

  case object CycleReset extends StatType {
    override def log: String = "Cycle reset at: %s"
  }
}
