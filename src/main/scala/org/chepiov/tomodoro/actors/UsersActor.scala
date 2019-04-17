package org.chepiov.tomodoro.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.chepiov.tomodoro.algebras.{ToFuture, UserChat}

class UsersActor[F[_]: ToFuture](chat: UserChat[F]) extends Actor with ActorLogging {
  import UsersActor._
  import context._

  override def receive: Receive = behavior(Map())

  private def behavior(users: Map[Long, (ActorRef, ActorRef)]): Receive = {
    case GetUser(chatId, ack) if users.contains(chatId) =>
      val (userActor, _) = users(chatId)
      log.debug(s"[$chatId] returning existed user: ${userActor.path}")
      ack(userActor)
    case GetUser(chatId, ack) =>
      val chatActor = actorOf(UserChatActor.props(chatId, chat), s"chat-$chatId")
      val userActor = actorOf(UserActor.props(chatId, system.actorSelection(chatActor.path)), s"user-$chatId")
      watch(chatActor)
      watch(userActor)
      log.debug(s"[$chatId] [re]creating user: ${userActor.path}")
      become(behavior(users + ((chatId, (userActor, chatActor)))))
      ack(userActor)
  }
}

case object UsersActor {
  def props[F[_]: ToFuture](chat: UserChat[F]): Props =
    Props(new UsersActor(chat))

  final case class GetUser(chatId: Long, ack: ActorRef => Unit)
}
