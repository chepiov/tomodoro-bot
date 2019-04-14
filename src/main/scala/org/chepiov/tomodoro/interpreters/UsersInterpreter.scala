package org.chepiov.tomodoro.interpreters

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.Sync
import org.chepiov.tomodoro.algebras.{Messenger, User, Users}
import org.chepiov.tomodoro.interpreters.actors.UsersActor
import org.chepiov.tomodoro.typeclasses.{FromFuture, ToFuture}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UsersInterpreter[F[_]: Sync: FromFuture: ToFuture](messenger: Messenger[F])(
    implicit system: ActorSystem
) extends Users[F] {
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val timeout: Timeout     = Timeout(5.seconds)

  private val usersActor = system.actorOf(UsersActor.props(messenger), "users")

  override def getOrCreateUser(chatId: Long): F[User[F]] = {
    println(usersActor)
    FromFuture[F].fromFuture {
      Sync[F].delay {
        (usersActor ? chatId).mapTo[ActorRef].map(r => new UserInterpreter[F](r))
      }
    }
  }
}

case object UsersInterpreter {
  def apply[F[_]: Sync: FromFuture: ToFuture](messenger: Messenger[F])(
      implicit actorSystem: ActorSystem
  ): F[Users[F]] =
    Sync[F].delay(new UsersInterpreter(messenger))
}
