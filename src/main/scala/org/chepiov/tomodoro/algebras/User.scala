package org.chepiov.tomodoro.algebras

import java.time.OffsetDateTime

import org.chepiov.tomodoro.interpreters.hooks.StatDescriptor
import simulacrum.typeclass

/**
  * Represents user of Tomodoro bot.
  *
  * @tparam F effect
  */
@typeclass
trait User[F[_]] {
  import User._

  /**
    * Advances user state accordingly command.
    *
    * @param cmd to use
    */
  def advance(cmd: UserCommand): F[Unit]

  /**
    * Handles query about user.
    *
    * @param query to use
    */
  def info(query: UserInfoQuery): F[Unit]

  /**
    * Handles sending user statistic.
    *
    * @param query to use
    */
  def stats(query: UserStatsResult): F[Unit]
}

case object User {

  /**
    * Represents user state.
    */
  final case class UserState(
      settings: UserSettings,
      status: Status,
      settingsUpdate: SettingsUpdate = NotUpdate
  )

  /**
    * Represents user settings.
    *
    * @param duration   duration of one tomodoro
    * @param shortBreak duration of short break
    * @param longBreak  duration of long break
    * @param amount     amount of tomodoroes in one cycle
    */
  final case class UserSettings(duration: Int, shortBreak: Int, longBreak: Int, amount: Int)

  /**
    * Represents user status.
    */
  sealed trait Status extends Product with Serializable {

    /**
      * Remaining amount of tomodoroes in current cycle.
      *
      * @return amount of tomodoroes
      */
    def remaining: Int

    /**
      * When user moved to this status.
      *
      * @return epoch seconds
      */
    def startTime: Long
  }

  /**
    * Status which should be finished after some period.
    */
  sealed trait FiniteStatus extends Status {

    /**
      * When the status should be completed.
      *
      * @return epoch seconds
      */
    def endTime: Long
  }

  /**
    * Status which was suspended.
    */
  sealed trait SuspendedStatus extends Status {
    def suspend: Long
  }

  /**
    * Waiting for the start of the next tomodoro.
    */
  final case class WaitingWork(remaining: Int, startTime: Long) extends Status

  /**
    * Waiting for the start of the next break.
    */
  final case class WaitingBreak(remaining: Int, startTime: Long) extends Status

  /**
    * Tomodoro working
    */
  final case class Working(remaining: Int, startTime: Long, endTime: Long) extends FiniteStatus

  /**
    * Tomodoro breaking.
    */
  final case class Breaking(remaining: Int, startTime: Long, endTime: Long) extends FiniteStatus

  /**
    * Current tomodoro suspended.
    */
  final case class WorkSuspended(remaining: Int, startTime: Long, suspend: Long) extends SuspendedStatus

  /**
    * Current break suspended.
    */
  final case class BreakSuspended(remaining: Int, startTime: Long, suspend: Long) extends SuspendedStatus

  /**
    * Settings update state.
    */
  sealed trait SettingsUpdate

  final case class DurationUpdate(startedAt: Long) extends SettingsUpdate

  final case class ShortBreakUpdate(startedAt: Long) extends SettingsUpdate

  final case class LongBreakUpdate(startedAt: Long) extends SettingsUpdate

  final case class AmountUpdate(startedAt: Long) extends SettingsUpdate

  case object NotUpdate extends SettingsUpdate

  /**
    * Commands for user.
    */
  sealed trait Command extends Product with Serializable {

    /**
      * Time (epoch seconds) of command.
      *
      * @return
      */
    def time: Long
  }

  /**
    * Commands for user, available outside (i.e. from Telegram client).
    */
  sealed trait UserCommand extends Command

  /**
    * Continue the waiting or suspended states.
    */
  final case class Continue(time: Long) extends UserCommand

  /**
    * Finish finite state (internal command, used by scheduler).
    */
  final case class Finish(time: Long) extends Command

  /**
    * Suspend finite state.
    */
  final case class Suspend(time: Long) extends UserCommand

  /**
    * Reset all state.
    */
  final case class Reset(time: Long) extends UserCommand

  /**
    * Skip finite state.
    */
  final case class Skip(time: Long) extends UserCommand

  /**
    * Update settings command.
    */
  final case class SetSettings(time: Long) extends UserCommand

  /**
    * Changing settings commands.
    */
  sealed trait ChangeSettingsCommand extends UserCommand

  final case class AwaitChangingDuration(time: Long) extends ChangeSettingsCommand

  final case class AwaitChangingLongBreak(time: Long) extends ChangeSettingsCommand

  final case class AwaitChangingShortBreak(time: Long) extends ChangeSettingsCommand

  final case class AwaitChangingAmount(time: Long) extends ChangeSettingsCommand

  final case class SetSettingsValue(time: Long, value: Int) extends ChangeSettingsCommand

  /**
    * User info queries.
    */
  sealed trait UserInfoQuery extends Product with Serializable

  case object GetState extends UserInfoQuery

  case object GetHelp extends UserInfoQuery

  case object GetStats extends UserInfoQuery

  /**
    * User statistic results.
    */
  final case class Log(
      chatId: Long,
      time: OffsetDateTime,
      descriptor: StatDescriptor,
      log: String
  )
  sealed trait UserStatsResult extends Product with Serializable

  final case class PushLog(page: Int, logs: List[Log]) extends UserStatsResult

  final case class PushCompletedLastDay(count: Int) extends UserStatsResult

  final case class PushCompletedLastWeek(count: Int) extends UserStatsResult

  final case class PushCompletedLastMonth(count: Int) extends UserStatsResult

}
