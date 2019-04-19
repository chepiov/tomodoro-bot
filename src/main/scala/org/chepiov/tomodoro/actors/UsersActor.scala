package org.chepiov.tomodoro.actors

import akka.actor.SupervisorStrategy.Restart
import akka.actor.{Actor, ActorLogging, ActorRef, OneForOneStrategy, Props, SupervisorStrategy}
import akka.pattern.{BackoffOpts, BackoffSupervisor}
import cats.effect.Effect
import org.chepiov.tomodoro.algebras.UserChat

import scala.concurrent.duration._

class UsersActor[F[_]: Effect](chat: UserChat[F]) extends Actor with ActorLogging {
  import UsersActor._
  import context._

  override def receive: Receive = behavior(Map(), Map())

  private def behavior(chatIdToUser: Map[Long, (ActorRef, ActorRef)], userToChatId: Map[ActorRef, Long]): Receive = {
    case GetUser(chatId, ack) if chatIdToUser.contains(chatId) =>
      val (userActor, _) = chatIdToUser(chatId)
      log.debug(s"[$chatId] returning existed user: ${userActor.path}")
      ack(userActor)
    case GetUser(chatId, ack) =>
      val (userActor, chatActor) = createUser(chatId)
      log.debug(s"[$chatId] [re]creating user: ${userActor.path}")
      become(behavior(chatIdToUser + ((chatId, (userActor, chatActor))), userToChatId + ((userActor, chatId))))
      ack(userActor)
  }

  private def createUser(chatId: Long): (ActorRef, ActorRef) = {
    val chatActor = actorOf(UserChatActor.props(chatId, chat), s"chat-$chatId")

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

    (userActorSupervisor, chatActor)
  }

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(loggingEnabled = true) {
    case _ => Restart
  }
}

case object UsersActor {
  def props[F[_]: Effect](chat: UserChat[F]): Props =
    Props(new UsersActor(chat))

  final case class GetUser(chatId: Long, ack: ActorRef => Unit)
}
