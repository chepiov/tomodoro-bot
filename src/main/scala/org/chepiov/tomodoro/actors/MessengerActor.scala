package org.chepiov.tomodoro.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import akka.util.Timeout
import cats.effect.Effect
import org.chepiov.tomodoro.actors.UserActor.{ChatMsg, ChatMsgConfirm}
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Represents messenger for user chat.
  *
  * @param chat messages sender
  */
class MessengerActor[F[_] : Effect](chat: TSendMessage => F[Try[Unit]]) extends Actor with ActorLogging {

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout = 5.seconds

  override def receive: Receive = {
    case a: ChatMsg =>
      Effect[F]
        .toIO(chat(a.msg))
        .unsafeToFuture()
        .filter {
          case Success(_) => true
          case Failure(e) =>
            log.error(e, s"Error during sending new message: ${a.msg} to user chat")
            false
        }
        .map(_ => ChatMsgConfirm(a.deliveryId)) pipeTo sender()
      ()
  }
}

case object MessengerActor {
  def props[F[_] : Effect](chat: TSendMessage => F[Try[Unit]]): Props =
    Props(new MessengerActor(chat))
}
