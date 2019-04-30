package org.chepiov.tomodoro.actors

import java.time.OffsetDateTime

import akka.actor.Status.Failure
import akka.actor.{ActorLogging, ActorSelection, Props, Timers}
import akka.persistence.AtLeastOnceDelivery.UnconfirmedWarning
import akka.persistence.{AtLeastOnceDelivery, PersistentActor, RecoveryCompleted, SnapshotOffer}
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.Users.defaultUserSettings
import org.chepiov.tomodoro.programs.UserActivity.StateChangedEvent
import org.chepiov.tomodoro.programs.UserStateMachine

import scala.concurrent.duration._
import scala.math.max

/**
  * Represents user.
  *
  * @param chatId          related user chat id
  * @param messenger       user messenger actor
  * @param timeUnit        time unit of users scheduling
  * @param defaultSettings settings for newly created user
  */
class UserActor(
    chatId: Long,
    messenger: ActorSelection,
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
      persist(MessageSentEvent(UserStateMachine.query(chatId, query, state)))(deliverMsg(ack))
    case ChatMsgConfirm(deliveryId) =>
      persist(MessageConfirmedEvent(deliveryId)) { evt =>
        confirmDelivery(evt.deliveryId)
        log.debug(s"[$chatId] Message delivered")
      }
    case ChatMsgError(deliveryId, e) =>
      log.error(e, s"Can't deliver message, deliveryId: $deliveryId")
    case Failure(e) =>
      log.error(e, "Probably can't deliver message")
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
          updateState(evt.state)
          if (lastSequenceNr % snapShotInterval == 0 && lastSequenceNr != 0)
            saveSnapshot(state)
          maybeMessage.foreach { message =>
            persist(MessageSentEvent(message))(deliverMsg())
          }
        }
      case (_, Some(message)) =>
        log.debug(s"[$chatId] State was not changed: $state")
        ack()
        persist(MessageSentEvent(message))(deliverMsg())
      case _ =>
        log.debug(s"[$chatId] Nothing to do")
        ack()
    }
  }

  private def deliverMsg(ack: () => Unit = () => ())(evt: MessageSentEvent): Unit = {
    log.debug(s"[$chatId] New message event persisted")
    ack()
    deliver(messenger)(deliveryId => ChatMsg(deliveryId, evt.message))
  }

  private[actors] def updateState(newState: UserState): Unit =
    state = newState

  override def receiveRecover: Receive = {
    case evt: StateChangedEvent                => updateState(evt.state)
    case SnapshotOffer(_, snapshot: UserState) => updateState(snapshot)
    case MessageSentEvent(message)             => deliver(messenger)(deliveryId => ChatMsg(deliveryId, message))
    case MessageConfirmedEvent(deliveryId)     => confirmDelivery(deliveryId); ()
    case RecoveryCompleted =>
      timerState(state.status)
      log.debug(s"[$chatId] Recovering completed. Current state: $state")
    case s => log.debug(s"DAFUUUCK: $s")
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

  final case class CommandMsg(cmd: Command, ask: () => Unit)

  final case class QueryMsg(query: UserInfoQuery, ask: () => Unit)

  final case class MessageSentEvent(message: TSendMessage)

  final case class MessageConfirmedEvent(deliveryId: Long)

  final case class ChatMsg(deliveryId: Long, msg: TSendMessage)

  sealed trait ChatMsgResult {
    def deliveryId: Long
  }

  final case class ChatMsgConfirm(deliveryId: Long) extends ChatMsgResult

  final case class ChatMsgError(deliveryId: Long, ex: Throwable) extends ChatMsgResult

  private def now: Long = OffsetDateTime.now().toEpochSecond

  private val timerKey: String = "FINISH"
}
