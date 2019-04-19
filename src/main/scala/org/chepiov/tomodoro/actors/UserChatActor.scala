package org.chepiov.tomodoro.actors

import akka.actor.{Actor, Props}
import akka.pattern.pipe
import akka.util.Timeout
import cats.effect.Effect
import org.chepiov.tomodoro.actors.UserActor.{ChatMsg, ChatMsgConfirm}
import org.chepiov.tomodoro.algebras.UserChat

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UserChatActor[F[_]: Effect](chatId: Long, userChat: UserChat[F]) extends Actor {

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout     = 5.seconds

  override def receive: Receive = {
    case a: ChatMsg =>
      Effect[F]
        .toIO(userChat.sayTo(chatId, a.msg))
        .unsafeToFuture()
        .filter(identity)
        .map(_ => ChatMsgConfirm(a.deliveryId)) pipeTo sender()
      ()
  }
}

case object UserChatActor {
  def props[F[_]: Effect](chatId: Long, messenger: UserChat[F]): Props =
    Props(new UserChatActor(chatId, messenger))
}
