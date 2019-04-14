package org.chepiov.tomodoro.interpreters.actors

import akka.actor.{Actor, Props}
import akka.pattern.pipe
import akka.util.Timeout
import org.chepiov.tomodoro.algebras.Messenger
import org.chepiov.tomodoro.algebras.User.Answer
import org.chepiov.tomodoro.typeclasses.ToFuture
import org.chepiov.tomodoro.typeclasses.ToFuture.ops._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UserMessengerActor[F[_]: ToFuture](chatId: Long, messenger: Messenger[F]) extends Actor {

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout     = 5.seconds

  override def receive: Receive = {
    case a: Answer =>
      messenger.sendToUser(chatId, a).toFuture.filter(identity).map(r => (chatId, r)) pipeTo sender()
      ()
  }
}

case object UserMessengerActor {
  def props[F[_]: ToFuture](chatId: Long, messenger: Messenger[F]): Props =
    Props(new UserMessengerActor(chatId, messenger))
}
