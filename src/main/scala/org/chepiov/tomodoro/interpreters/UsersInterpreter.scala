package org.chepiov.tomodoro.interpreters

import akka.actor.{ActorRef, ActorSystem}
import cats.effect.{Async, Effect}
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{Applicative, ApplicativeError, Id}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.actors.UsersActor
import org.chepiov.tomodoro.actors.UsersActor.GetUser
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.algebras.{Repository, Telegram, User, Users}
import org.chepiov.tomodoro.programs.UserActivity
import org.chepiov.tomodoro.programs.UserActivity.StateChangedEvent

import scala.util.{Failure, Success, Try}

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
      telegram: Telegram[F],
      repository: Repository[F],
      actorSystem: ActorSystem
  ): I[Users[F]] = {
    for {
      _          <- Applicative[I].unit
      usersActor = actorSystem.actorOf(UsersActor.props(chat(telegram), statistic(repository)), "users")
    } yield new UsersInterpreter(usersActor)
  }

  def apply[F[_]: Effect](
      telegram: Telegram[F],
      repository: Repository[F],
      actorSystem: ActorSystem
  ): F[Users[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      u                            <- apply[F, F](telegram, repository, actorSystem)
    } yield u

  private def chat[F[_]: ApplicativeError[?[_], Throwable]](telegram: Telegram[F]): TSendMessage => F[Try[Unit]] =
    msg =>
      telegram.sendMessage(msg) *> Applicative[F].pure(Try(())).handleErrorWith { e =>
        Applicative[F].pure(Failure(e))
      }

  private def statistic[F[_]: ApplicativeError[?[_], Throwable]](
      repository: Repository[F]
  ): StateChangedEvent => F[Try[Unit]] =
    event => {
      UserActivity.createLog(event) match {
        case Some(log) =>
          val result = repository.addLog(log)
          result *> Applicative[F].pure(Try(())).handleErrorWith { e =>
            Applicative[F].pure(Failure(e))
          }
        case None => Applicative[F].pure(Success(()))
      }
    }
}
