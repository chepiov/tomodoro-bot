package org.chepiov.tomodoro.interpreters.actors

import java.time.OffsetDateTime

import akka.actor.{ActorSelection, Props, Timers}
import akka.event.Logging
import akka.persistence.{PersistentActor, RecoveryCompleted, SnapshotOffer}
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.Users.defaultUserSettings
import org.chepiov.tomodoro.programs.UserStateMachine

import scala.concurrent.duration._
import scala.math.max

class UserActor(chatId: Long, chat: ActorSelection, timeUnit: TimeUnit, snapShotInterval: Int)
    extends Timers with PersistentActor {
  import UserActor._
  import UserStateMachine._

  val log = Logging(context.system.eventStream, "user-actor")

  //noinspection ActorMutableStateInspection
  private var state: UserState = UserState(defaultUserSettings, WaitingWork(defaultUserSettings.amount, now))

  override def persistenceId: String = chatId.toString

  override def receiveCommand: Receive = {
    case cmd: Command =>
      advance(cmd, timeUnit).run(state).value match {
        case (s, a) =>
          timerState(s.status)
          persist(UserEvent(chatId, s)) { evt =>
            log.debug(s"[$chatId] Event persisted: $evt")
            state = evt.state
            if (lastSequenceNr % snapShotInterval == 0 && lastSequenceNr != 0)
              saveSnapshot(state)
            chat ! a
            context.sender() ! accepted
          }
      }
    case GetState =>
      log.debug(s"[$chatId] State requested")
      context.sender() ! accepted
    case GetHelp =>
      log.debug(s"[$chatId] Help requested")
      context.sender() ! accepted
  }

  override def receiveRecover: Receive = {
    case evt: UserEvent =>
      state = evt.state
    case SnapshotOffer(_, snapshot: UserState) => state = snapshot
    case RecoveryCompleted =>
      timerState(state.status)
      log.debug(s"[$chatId] Recovering completed. Current state: $state")
  }

  private def timerState(status: UserStatus): Unit =
    status match {
      case s: FiniteUserStatus =>
        val currentTime = now
        val time        = if (s.end < currentTime) currentTime else s.end
        val duration    = max(s.end - currentTime, 0)
        timers.startSingleTimer(timerKey, Finish(time), FiniteDuration(duration, SECONDS))
      case _ => timers.cancel(timerKey)
    }
}

case object UserActor {

  def props(chatId: Long, chat: ActorSelection, timeUnit: TimeUnit = MINUTES, snapshotInterval: Int = 1000): Props =
    Props(new UserActor(chatId, chat, timeUnit, snapshotInterval))

  final case class UserEvent(chatId: Long, state: UserState)

  private def now: Long = OffsetDateTime.now().toEpochSecond

  private val timerKey: String = "FINISH"

  val accepted = "accepted"
}
