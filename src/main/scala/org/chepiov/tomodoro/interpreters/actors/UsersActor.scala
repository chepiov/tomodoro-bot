package org.chepiov.tomodoro.interpreters.actors

import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging
import org.chepiov.tomodoro.algebras.Messenger
import org.chepiov.tomodoro.typeclasses.ToFuture

class UsersActor[F[_]: ToFuture](messenger: Messenger[F]) extends Actor {
  import context._

  val log = Logging(system.eventStream, "users-actor")

  override def receive: Receive = behavior(Map())

  private def behavior(users: Map[Long, (ActorRef, ActorRef)]): Receive = {
    case chatId: Long if users.contains(chatId) =>
      val (userActor, _) = users(chatId)
      log.debug(s"[$chatId] returning existed user: ${userActor.path}")
      sender() ! userActor
    case chatId: Long =>
      val messengerActor = actorOf(UserMessengerActor.props(chatId, messenger), s"messenger-$chatId")
      val userActor      = actorOf(UserActor.props(chatId, system.actorSelection(messengerActor.path)))
      watch(messengerActor)
      watch(userActor)
      log.debug(s"[$chatId] [re]creating user: ${userActor.path}")
      become(behavior(users + ((chatId, (userActor, messengerActor)))))
      sender() ! userActor
  }
}

object UsersActor {
  def props[F[_]: ToFuture](messenger: Messenger[F]): Props =
    Props(new UsersActor(messenger))
}
