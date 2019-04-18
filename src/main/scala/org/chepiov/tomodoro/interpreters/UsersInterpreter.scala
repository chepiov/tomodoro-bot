package org.chepiov.tomodoro.interpreters

import akka.actor.{ActorRef, ActorSystem}
import cats.effect.Async
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Id}
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.actors.UsersActor
import org.chepiov.tomodoro.actors.UsersActor.GetUser
import org.chepiov.tomodoro.algebras.{ToFuture, User, UserChat, Users}

class UsersInterpreter[F[_]: Logger: Async](usersActor: ActorRef) extends Users[F] {

  override def getOrCreateUser(chatId: Long): F[User[F]] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Searching user")
      user <- Async[F].async[User[F]] { k =>
               usersActor ! GetUser(chatId, ref => k(Right(UserInterpreter[Id, F](chatId, ref))))
             }
    } yield user
}

case object UsersInterpreter {
  def apply[I[_]: Applicative, F[_]: Logger: Async: ToFuture](
      userChat: UserChat[F],
      actorSystem: ActorSystem
  ): I[Users[F]] = {
    for {
      _          <- Applicative[I].unit
      usersActor = actorSystem.actorOf(UsersActor.props(userChat), "users")
    } yield new UsersInterpreter(usersActor)
  }

  def apply[F[_]: Logger: Async: ToFuture](
      userChat: UserChat[F],
      actorSystem: ActorSystem
  ): F[Users[F]] = apply[F, F](userChat, actorSystem)
}