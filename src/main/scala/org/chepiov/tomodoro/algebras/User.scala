package org.chepiov.tomodoro.algebras

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
}

case object User {

  /**
    * Represents user state.
    */
  final case class UserState(settings: UserSettings, status: UserStatus)

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
  sealed trait UserStatus extends Product with Serializable {

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
  sealed trait FiniteUserStatus extends UserStatus {

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
  sealed trait SuspendedUserStatus extends UserStatus {
    def suspend: Long
  }

  /**
    * Waiting for the start of the next tomodoro.
    */
  final case class WaitingWork(remaining: Int, startTime: Long) extends UserStatus

  /**
    * Waiting for the start of the next break.
    */
  final case class WaitingBreak(remaining: Int, startTime: Long) extends UserStatus

  /**
    * Tomodoro working
    */
  final case class Working(remaining: Int, startTime: Long, endTime: Long) extends FiniteUserStatus

  /**
    * Tomodoro breaking.
    */
  final case class Breaking(remaining: Int, startTime: Long, endTime: Long) extends FiniteUserStatus

  /**
    * Current tomodoro suspended.
    */
  final case class WorkSuspended(remaining: Int, startTime: Long, suspend: Long) extends SuspendedUserStatus

  /**
    * Current break suspended.
    */
  final case class BreakSuspended(remaining: Int, startTime: Long, suspend: Long) extends SuspendedUserStatus

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
    * Setting new settings command.
    */
  final case class SetSettings(time: Long, settings: UserSettings) extends UserCommand

  final case class Continue(time: Long) extends UserCommand

  final case class Finish(time: Long) extends Command

  final case class Suspend(time: Long) extends UserCommand

  final case class Stop(time: Long) extends UserCommand

  final case class Skip(time: Long) extends UserCommand

  sealed trait UserInfoQuery extends Product with Serializable

  case object GetState extends UserInfoQuery

  case object GetHelp extends UserInfoQuery

  sealed trait Answer extends Product with Serializable

  final case class Correct(availableCommands: Set[Command])

  case object Ok extends Answer

  case object AlreadyInProgress extends Answer

  case object NotYetInProgress extends Answer

  case object InvalidTime extends Answer

  case object IllegalState extends Answer

}
