package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram.TMessage
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.{Manager, Users}

class ManagerInterpreter[F[_]: Logger: Monad](users: Users[F]) extends Manager[F] {
  import ManagerInterpreter._

  override def decide(message: TMessage): F[Unit] =
    message.text.getOrElse("") match {
      case "/help" =>
        for {
          _    <- Logger[F].debug("Received help request")
          user <- users.getOrCreateUser(message.chat.id)
          r    <- user.info(GetHelp)
        } yield r
      case "/continue" =>
        for {
          _    <- Logger[F].debug(s"[${message.chat.id}] Received continue command")
          user <- users.getOrCreateUser(message.chat.id)
          r    <- user.advance(Continue(now))
        } yield r
      case "/suspend" =>
        for {
          _    <- Logger[F].debug(s"[${message.chat.id}] Received suspend command")
          user <- users.getOrCreateUser(message.chat.id)
          r    <- user.advance(Suspend(now))
        } yield r
      case "/stop" =>
        for {
          _    <- Logger[F].debug(s"[${message.chat.id}] Received stop command")
          user <- users.getOrCreateUser(message.chat.id)
          r    <- user.advance(Stop(now))
        } yield r
      case "/skip" =>
        for {
          _    <- Logger[F].debug(s"[${message.chat.id}] Received skip command")
          user <- users.getOrCreateUser(message.chat.id)
          r    <- user.advance(Skip(now))
        } yield r
      case m =>
        for {
          _    <- Logger[F].warn(s"[${message.chat.id}] Received unknown message: $m")
          user <- users.getOrCreateUser(message.chat.id)
          r    <- user.info(GetHelp)
        } yield r
    }
}

object ManagerInterpreter {

  def apply[I[_]: Monad, F[_]: Logger: Monad](users: Users[F]): I[Manager[F]] =
    for {
      _ <- Monad[I].unit
      m = new ManagerInterpreter[F](users)
    } yield m

  def apply[F[_]: Logger: Monad](users: Users[F]): F[Manager[F]] = apply[F, F](users)

  private def now: Long = OffsetDateTime.now().toEpochSecond
}
