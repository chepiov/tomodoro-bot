package org.chepiov.tomodoro.actors

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorLogging, ActorRef, OneForOneStrategy, Props, SupervisorStrategy}
import akka.pattern.{BackoffOpts, BackoffSupervisor}
import cats.effect.Effect
import org.chepiov.tomodoro.actors.UserActor.StateChangedEvent
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage

import scala.concurrent.duration._
import scala.util.Try

class UsersActor[F[_]: Effect](chat: TSendMessage => F[Try[Unit]], stat: StateChangedEvent => F[Try[Unit]])
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
    val chatActor = actorOf(UserChatActor.props(chat), s"chat-$chatId")

    val userActorProps = UserActor.props(chatId, system.actorSelection(chatActor.path))
    val userActorSupervisorProps = BackoffSupervisor.props(
      BackoffOpts.onStop(
        userActorProps,
        childName = s"user-$chatId",
        minBackoff = 3.seconds,
        maxBackoff = 30.seconds,
        randomFactor = 0.2
      )
    )
    val userActorSupervisor = context.actorOf(userActorSupervisorProps, name = s"user-supervisor-$chatId")

    val userStatActorProps = UserStatActor.props(chatId, stat)
    val userStatActorSupervisorProps = BackoffSupervisor.props(
      BackoffOpts.onStop(
        userStatActorProps,
        childName = s"user-stat-$chatId",
        minBackoff = 3.seconds,
        maxBackoff = 30.seconds,
        randomFactor = 0.2
      )
    )
    context.actorOf(userStatActorSupervisorProps, name = s"user-stat-supervisor-$chatId")

    userActorSupervisor
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
