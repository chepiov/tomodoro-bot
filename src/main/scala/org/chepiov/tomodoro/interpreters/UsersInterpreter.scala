package org.chepiov.tomodoro.interpreters

import akka.actor.{ActorRef, ActorSystem}
import cats.effect.{Async, Effect}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, Id}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.actors.UsersActor
import org.chepiov.tomodoro.actors.UsersActor.GetUser
import org.chepiov.tomodoro.algebras.{User, UserChat, Users}

class UsersInterpreter[F[_]: Logger: Effect](usersActor: ActorRef) extends Users[F] {

  override def getOrCreateUser(chatId: Long): F[User[F]] =
    for {
      _ <- Logger[F].debug(s"[$chatId] Searching user")
      user <- Async[F].async[User[F]] { k =>
               usersActor ! GetUser(chatId, ref => k(Right(UserInterpreter[Id, F](chatId, ref))))
             }
    } yield user
}

case object UsersInterpreter {
  def apply[I[_]: Applicative, F[_]: Logger: Effect](
      userChat: UserChat[F],
      actorSystem: ActorSystem
  ): I[Users[F]] = {
    for {
      _          <- Applicative[I].unit
      usersActor = actorSystem.actorOf(UsersActor.props(userChat), "users")
    } yield new UsersInterpreter(usersActor)
  }

  def apply[F[_]: Effect](
      userChat: UserChat[F],
      actorSystem: ActorSystem
  ): F[Users[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      u                            <- apply[F, F](userChat, actorSystem)
    } yield u
}
