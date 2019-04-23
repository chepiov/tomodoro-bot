package org.chepiov.tomodoro.actors

import java.time.OffsetDateTime

import akka.actor.{ActorLogging, ActorSelection, Props, Timers}
import akka.persistence.AtLeastOnceDelivery.UnconfirmedWarning
import akka.persistence.{AtLeastOnceDelivery, PersistentActor, RecoveryCompleted, SnapshotOffer}
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
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
) extends Timers with PersistentActor with AtLeastOnceDelivery with ActorLogging {
  import UserActor._

  //noinspection ActorMutableStateInspection
  private var state: UserState = UserState(defaultSettings, WaitingWork(defaultSettings.amount, now), NotUpdate)

  override def persistenceId: String = s"user-$chatId"

  override def receiveCommand: Receive = {
    case CommandMsg(cmd, ack) =>
      receiveCommand(cmd, ack)
    case cmd: Finish =>
      receiveCommand(cmd, () => ())
    case QueryMsg(query, ack) =>
      log.debug(s"[$chatId] Query $query received")
      persist(MessageSentEvent(UserStateMachine.query(chatId, query, state)))(deliverAfterPersist(ack))
    case StatsMsg(query, ack) =>
      log.debug(s"[$chatId] Stats push $query received")
      persist(MessageSentEvent(UserStateMachine.stats(chatId, query)))(deliverAfterPersist(ack))
    case ChatMsgConfirm(deliveryId) =>
      persist(MessageConfirmedEvent(deliveryId)) { evt =>
        confirmDelivery(evt.deliveryId)
        log.debug(s"[$chatId] Message delivered")
      }
    case UnconfirmedWarning(unconfirmed) =>
      log.warning(s"[$chatId] There are messages which can't be delivered to user chat, skipping")
      unconfirmed.foreach(u => confirmDelivery(u.deliveryId))
  }

  private def receiveCommand(cmd: Command, ack: () => Unit): Unit = {
    log.debug(s"[$chatId] Command $cmd received")
    val result = UserStateMachine.advance(chatId, cmd, timeUnit).run(state).value
    result match {
      case (s, maybeMessage) if s != state =>
        timerState(s.status)
        persist(StateChangedEvent(chatId, s, cmd)) { evt =>
          log.debug(s"[$chatId] State event persisted: ${evt.state}")
          ack()
          state = evt.state
          if (lastSequenceNr % snapShotInterval == 0 && lastSequenceNr != 0)
            saveSnapshot(state)
          maybeMessage.foreach { message =>
            persist(MessageSentEvent(message))(deliverAfterPersist())
          }
        }
      case (_, Some(message)) =>
        log.debug(s"[$chatId] State was not changed: $state")
        ack()
        persist(MessageSentEvent(message))(deliverAfterPersist())
    }
  }

  private def deliverAfterPersist(ack: () => Unit = () => ())(evt: MessageSentEvent): Unit = {
    log.debug(s"[$chatId] Message event persisted")
    ack()
    deliver(userChat)(deliveryId => ChatMsg(deliveryId, evt.message))
  }

  override def receiveRecover: Receive = {
    case evt: StateChangedEvent                => state = evt.state
    case SnapshotOffer(_, snapshot: UserState) => state = snapshot
    case MessageSentEvent(message)             => deliver(userChat)(deliveryId => ChatMsg(deliveryId, message))
    case MessageConfirmedEvent(deliveryId)     => confirmDelivery(deliveryId); ()
    case RecoveryCompleted =>
      timerState(state.status)
      log.debug(s"[$chatId] Recovering completed. Current state: $state")
  }

  private def timerState(status: Status): Unit =
    status match {
      case s: FiniteStatus =>
        val currentTime = now
        val time        = if (s.endTime < currentTime) currentTime else s.endTime
        val duration    = max(s.endTime - currentTime, 0)
        log.debug(s"[$chatId] Scheduling timer, finish after ${FiniteDuration(duration, SECONDS)}")
        timers.startSingleTimer(timerKey, Finish(time), FiniteDuration(duration, SECONDS))
      case _ =>
        if (timers.isTimerActive(timerKey)) {
          log.debug(s"[$chatId] Cancelling timer")
          timers.cancel(timerKey)
        }
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

  final case class StateChangedEvent(chatId: Long, state: UserState, cmd: Command)

  final case class CommandMsg(cmd: Command, ask: () => Unit)
  final case class QueryMsg(query: UserInfoQuery, ask: () => Unit)
  final case class StatsMsg(query: UserStatsResult, ask: () => Unit)

  final case class MessageSentEvent(message: TSendMessage)
  final case class MessageConfirmedEvent(deliveryId: Long)

  final case class ChatMsg(deliveryId: Long, msg: TSendMessage)
  final case class ChatMsgConfirm(deliveryId: Long)

  private def now: Long = OffsetDateTime.now().toEpochSecond

  private val timerKey: String = "FINISH"
}
