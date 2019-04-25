package org.chepiov.tomodoro.algebras

import java.time.OffsetDateTime

import enumeratum.EnumEntry.Uppercase
import enumeratum.{Enum, EnumEntry}

import scala.collection.immutable

/**
  * Represents repository of Tomodoro bot data.
  *
  * @tparam F effect
  */
trait Repository[F[_]] {
  import Repository._

  /**
    * Finds user activity, ordered by time (desc).
    *
    * @param chatId user chat id
    * @param offset of logs
    * @param limit  of logs
    * @return
    */
  def findLogs(chatId: Long, offset: Int, limit: Int): F[List[ActivityLog]]

  /**
    * Adds new activity log
    *
    * @param log to add
    */
  def addLog(log: ActivityLog): F[Unit]

  /**
    * Counts completed by user tomodoroes in time period.
    *
    * @param chatId user chat id
    * @param from   start period
    * @param to     end period
    */
  def countCompleted(chatId: Long, from: OffsetDateTime, to: OffsetDateTime): F[Int]
}

case object Repository {

  final case class ActivityLog(
      chatId: Long,
      time: OffsetDateTime,
      descriptor: ActivityDescriptor,
      log: String
  )

  sealed trait ActivityDescriptor extends EnumEntry with Uppercase {
    def log: String
  }

  object ActivityDescriptor extends Enum[ActivityDescriptor] {
    override def values: immutable.IndexedSeq[ActivityDescriptor] = findValues

    case object CycleFinished extends ActivityDescriptor {
      override def log: String = "Cycle finished at: %s"
    }

    case object TomodoroFinished extends ActivityDescriptor {
      override def log: String = "Tomodoro finished at: %s, remaining tomodoroes: %d"
    }

    case object ShortBreakFinished extends ActivityDescriptor {
      override def log: String = "Short break finished at: %s"
    }

    case object LongBreakFinished extends ActivityDescriptor {
      override def log: String = "Long break finished at: %s"
    }

    case object TomodoroPaused extends ActivityDescriptor {
      override def log: String = "Tomodoro paused at: %s"
    }

    case object BreakPaused extends ActivityDescriptor {
      override def log: String = "Break paused at: %s"
    }

    case object TomodoroStarted extends ActivityDescriptor {
      override def log: String = "Tomodoro started or continued at: %s, remaining tomodoroes: %d"
    }

    case object ShortBreakStarted extends ActivityDescriptor {
      override def log: String = "Short break started or continued at: %s"
    }

    case object LongBreakStarted extends ActivityDescriptor {
      override def log: String = "Long break started or continued at: %s"
    }

    case object TomodoroSkipped extends ActivityDescriptor {
      override def log: String = "Tomodoro skipped at: %s"
    }

    case object BreakSkipped extends ActivityDescriptor {
      override def log: String = "Break skipped at: %s"
    }

    case object SettingsUpdated extends ActivityDescriptor {
      override def log: String = "Settings updated at: %s"
    }

    case object CycleReset extends ActivityDescriptor {
      override def log: String = "Cycle reset at: %s"
    }
  }
}
