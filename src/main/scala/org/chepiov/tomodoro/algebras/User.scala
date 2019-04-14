package org.chepiov.tomodoro.algebras

trait User[F[_]] {
  import User._

  def advance(cmd: UserCommand): F[Unit]
  def info(query: UserInfoQuery): F[Unit]
}

case object User {

  final case class UserState(settings: UserSettings, status: UserStatus)
  final case class UserSettings(duration: Int, shortBreak: Int, longBreak: Int, amount: Int)

  sealed trait UserStatus extends Product with Serializable {
    def remaining: Int
    def start: Long
  }
  sealed trait FiniteUserStatus extends UserStatus {
    def end: Long
  }
  sealed trait SuspendedUserStatus extends UserStatus {
    def suspend: Long
  }
  final case class WaitingWork(remaining: Int, start: Long)                   extends UserStatus
  final case class WaitingBreak(remaining: Int, start: Long)                  extends UserStatus
  final case class Working(remaining: Int, start: Long, end: Long)            extends FiniteUserStatus
  final case class Breaking(remaining: Int, start: Long, end: Long)           extends FiniteUserStatus
  final case class WorkSuspended(remaining: Int, start: Long, suspend: Long)  extends SuspendedUserStatus
  final case class BreakSuspended(remaining: Int, start: Long, suspend: Long) extends SuspendedUserStatus

  sealed trait Command extends Product with Serializable {
    def time: Long
  }
  sealed trait UserCommand                                         extends Command
  final case class SetSettings(time: Long, settings: UserSettings) extends UserCommand
  final case class Continue(time: Long)                            extends UserCommand
  final case class Finish(time: Long)                              extends Command
  final case class Suspend(time: Long)                             extends UserCommand
  final case class Stop(time: Long)                                extends UserCommand

  sealed trait UserInfoQuery extends Product with Serializable
  case object GetState       extends UserInfoQuery
  case object GetHelp        extends UserInfoQuery

  sealed trait Answer           extends Product with Serializable
  case object Ok                extends Answer
  case object AlreadyInProgress extends Answer
  case object NotYetInProgress  extends Answer
  case object InvalidTime       extends Answer
  case object IllegalState      extends Answer
}
