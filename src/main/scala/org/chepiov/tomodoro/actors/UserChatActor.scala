package org.chepiov.tomodoro.actors

import akka.actor.{Actor, Props}
import akka.pattern.pipe
import akka.util.Timeout
import org.chepiov.tomodoro.algebras.User.Answer
import org.chepiov.tomodoro.algebras.{ToFuture, UserChat}
import org.chepiov.tomodoro.algebras.ToFuture.ops._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UserChatActor[F[_]: ToFuture](chatId: Long, userChat: UserChat[F]) extends Actor {

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout     = 5.seconds

  override def receive: Receive = {
    case a: Answer =>
      userChat.sayTo(chatId, a).toFuture.filter(identity).map(r => (chatId, r)) pipeTo sender()
      ()
  }
}

case object UserChatActor {
  def props[F[_]: ToFuture](chatId: Long, messenger: UserChat[F]): Props =
    Props(new UserChatActor(chatId, messenger))
}
