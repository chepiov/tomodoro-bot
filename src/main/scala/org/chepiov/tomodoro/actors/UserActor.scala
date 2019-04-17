package org.chepiov.tomodoro.actors

import java.time.OffsetDateTime

import akka.actor.{ActorLogging, ActorSelection, Props, Timers}
import akka.persistence.{PersistentActor, RecoveryCompleted, SnapshotOffer}
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.Users.defaultUserSettings
import org.chepiov.tomodoro.programs.UserStateMachine

import scala.concurrent.duration._
import scala.math.max

class UserActor(
    chatId: Long,
    userChat: ActorSelection,
    timeUnit: TimeUnit,
    defaultSettings: UserSettings,
    snapShotInterval: Int
) extends Timers with PersistentActor with ActorLogging {
  import UserActor._
  import UserStateMachine._

  //noinspection ActorMutableStateInspection
  private var state: UserState = UserState(defaultSettings, WaitingWork(defaultSettings.amount, now))

  override def persistenceId: String = chatId.toString

  override def receiveCommand: Receive = {
    case CommandMsg(cmd, ask) =>
      advance(cmd, timeUnit).run(state).value match {
        case (s, a) =>
          timerState(s.status)
          persist(UserEvent(chatId, s)) { evt =>
            log.debug(s"[$chatId] Event persisted: $evt")
            state = evt.state
            if (lastSequenceNr % snapShotInterval == 0 && lastSequenceNr != 0)
              saveSnapshot(state)
            userChat ! a
            ask()
          }
      }
    case QueryMsg(GetState, ask) =>
      log.debug(s"[$chatId] State requested")
      userChat ! state
      ask()
    case QueryMsg(GetHelp, ask) =>
      log.debug(s"[$chatId] Help requested")
      ask()
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
        val time        = if (s.endTime < currentTime) currentTime else s.endTime
        val duration    = max(s.endTime - currentTime, 0)
        timers.startSingleTimer(timerKey, Finish(time), FiniteDuration(duration, SECONDS))
      case _ => timers.cancel(timerKey)
    }
}

case object UserActor {

  def props(
      chatId: Long,
      chat: ActorSelection,
      timeUnit: TimeUnit = MINUTES,
      defaultSettings: UserSettings = defaultUserSettings,
      snapshotInterval: Int = 1000
  ): Props =
    Props(new UserActor(chatId, chat, timeUnit, defaultSettings, snapshotInterval))

  final case class CommandMsg(cmd: Command, ask: () => Unit)
  final case class QueryMsg(query: UserInfoQuery, ask: () => Unit)

  final case class UserEvent(chatId: Long, state: UserState)

  private def now: Long = OffsetDateTime.now().toEpochSecond

  private val timerKey: String = "FINISH"

  val accepted: String = "accepted"
}
