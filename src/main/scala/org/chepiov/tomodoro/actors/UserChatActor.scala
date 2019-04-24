package org.chepiov.tomodoro.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import akka.util.Timeout
import cats.effect.Effect
import org.chepiov.tomodoro.actors.UserActor.{ChatEditMsg, ChatMsg, ChatMsgConfirm}
import org.chepiov.tomodoro.interpreters.hooks.UserChat

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class UserChatActor[F[_]: Effect](chat: UserChat[F]) extends Actor with ActorLogging {

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout     = 5.seconds

  override def receive: Receive = {
    case a: ChatMsg =>
      Effect[F]
        .toIO(chat.sayTo(a.msg))
        .unsafeToFuture()
        .filter {
          case Success(_) => true
          case Failure(e) =>
            log.error(e, s"Error during sending new message: ${a.msg} to user chat")
            false
        }
        .map(_ => ChatMsgConfirm(a.deliveryId)) pipeTo sender()
      ()
    case a: ChatEditMsg =>
      Effect[F]
        .toIO(chat.sayTo(a.msg))
        .unsafeToFuture()
        .filter {
          case Success(_) => true
          case Failure(e) =>
            log.error(e, s"Error during sending edited message: ${a.msg} to user chat")
            false
        }
        .map(_ => ChatMsgConfirm(a.deliveryId)) pipeTo sender()
      ()
  }
}

case object UserChatActor {
  def props[F[_]: Effect](chat: UserChat[F]): Props =
    Props(new UserChatActor(chat))
}
