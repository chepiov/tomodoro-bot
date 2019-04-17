package org.chepiov.tomodoro.interpreters

import akka.actor.ActorRef
import cats.Monad
import cats.effect.Async
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.actors.UserActor.{CommandMsg, QueryMsg}
import org.chepiov.tomodoro.algebras.User
import org.chepiov.tomodoro.algebras.User._

class UserInterpreter[F[_]: Logger: Async](chatId: Long, userActor: ActorRef) extends User[F] {

  override def advance(cmd: UserCommand): F[Unit] =
    for {
      _ <- Logger[F].debug(s"s[$chatId] Advancing user, command: $cmd")
      ack <- Async[F].async[Unit] { k =>
              userActor ! CommandMsg(cmd, () => k(Right(())))
            }
    } yield ack

  override def info(query: UserInfoQuery): F[Unit] =
    for {
      _ <- Logger[F].debug(s"s[$chatId] Querying user, query: $query")
      ack <- Async[F].async[Unit] { k =>
              userActor ! QueryMsg(query, () => k(Right(())))
            }
    } yield ack
}

case object UserInterpreter {
  def apply[I[_]: Monad, F[_]: Logger: Async](
      chatId: Long,
      userActor: ActorRef
  ): I[User[F]] =
    for {
      _ <- Monad[I].unit
      u = new UserInterpreter[F](chatId, userActor)
    } yield u
}
