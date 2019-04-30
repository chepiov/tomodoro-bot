package org.chepiov.tomodoro.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.pattern.pipe
import akka.util.Timeout
import cats.effect.Effect
import cats.effect.syntax.effect._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import org.chepiov.tomodoro.actors.UserActor.{ChatMsg, ChatMsgConfirm, ChatMsgError, ChatMsgResult}
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Represents messenger for user chat.
  *
  * @param messenger messages sender
  */
class MessengerActor[F[_]: Effect](messenger: TSendMessage => F[Unit]) extends Actor with ActorLogging {

  implicit val ec: ExecutionContext = context.dispatcher
  implicit val timeout: Timeout     = 5.seconds

  override def receive: Receive = {
    case chatMsg: ChatMsg =>
      sendMsg(chatMsg).toIO.unsafeToFuture() pipeTo sender()
      ()
  }

  private def sendMsg(chatMsg: ChatMsg): F[ChatMsgResult] =
    (messenger(chatMsg.msg) *> makeConfirmMsg(chatMsg)).handleErrorWith(makeErrorMsg(chatMsg))

  private def makeConfirmMsg(chatMsg: ChatMsg): F[ChatMsgResult] =
    (ChatMsgConfirm(chatMsg.deliveryId): ChatMsgResult).pure

  private def makeErrorMsg(chatMsg: ChatMsg)(e: Throwable): F[ChatMsgResult] =
    Effect[F].delay(log.error(e, s"Error during sending new message: ${chatMsg.msg} to user")) *>
      (ChatMsgError(chatMsg.deliveryId, e): ChatMsgResult).pure
}

case object MessengerActor {
  def props[F[_]: Effect](chat: TSendMessage => F[Unit]): Props =
    Props(new MessengerActor(chat))
}
