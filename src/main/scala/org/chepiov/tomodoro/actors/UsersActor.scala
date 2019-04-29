package org.chepiov.tomodoro.actors

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorLogging, ActorRef, OneForOneStrategy, Props, SupervisorStrategy}
import akka.pattern.{BackoffOpts, BackoffSupervisor}
import cats.effect.Effect
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.programs.UserActivity.StateChangedEvent

import scala.concurrent.duration._
import scala.util.Try

/**
  * Represents aggregation root of users
  *
  * @param messenger user messenger
  * @param activity user activity recorder
  */
class UsersActor[F[_]: Effect](messenger: TSendMessage => F[Try[Unit]], activity: StateChangedEvent => F[Try[Unit]])
    extends Actor with ActorLogging {

  import UsersActor._
  import context._

  override def receive: Receive = behavior(Map(), Map())

  private def behavior(chatIdToUser: Map[Long, ActorRef], userToChatId: Map[ActorRef, Long]): Receive = {
    case GetUser(chatId, ack) if chatIdToUser.contains(chatId) =>
      val userActor = chatIdToUser(chatId)
      log.debug(s"[$chatId] returning existed user: ${userActor.path}")
      ack(userActor)
    case GetUser(chatId, ack) =>
      val userActor = createUser(chatId)
      log.debug(s"[$chatId] [re]creating user: ${userActor.path}")
      become(behavior(chatIdToUser + ((chatId, userActor)), userToChatId + ((userActor, chatId))))
      ack(userActor)
  }

  private def createUser(chatId: Long): ActorRef = {
    val messengerActor = actorOf(MessengerActor.props(messenger), s"messenger-$chatId")

    val userActivityActorProps = UserActivityActor.props(chatId, activity)
    val userActivityActorSupervisorProps = BackoffSupervisor.props(
      BackoffOpts.onStop(
        userActivityActorProps,
        childName = s"user-activity-$chatId",
        minBackoff = 3.seconds,
        maxBackoff = 30.seconds,
        randomFactor = 0.2
      )
    )
    context.actorOf(userActivityActorSupervisorProps, name = s"user-activity-supervisor-$chatId")

    val userActorProps = UserActor.props(chatId, system.actorSelection(messengerActor.path))
    val userActorSupervisorProps = BackoffSupervisor.props(
      BackoffOpts.onStop(
        userActorProps,
        childName = s"user-$chatId",
        minBackoff = 3.seconds,
        maxBackoff = 30.seconds,
        randomFactor = 0.2
      )
    )
    context.actorOf(userActorSupervisorProps, name = s"user-supervisor-$chatId")
  }

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(loggingEnabled = true) {
    case _ => Restart
  }
}

case object UsersActor {

  def props[F[_]: Effect](chat: TSendMessage => F[Try[Unit]], stat: StateChangedEvent => F[Try[Unit]]): Props =
    Props(new UsersActor(chat, stat))

  final case class GetUser(chatId: Long, ack: ActorRef => Unit)
}
