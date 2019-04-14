package org.chepiov.tomodoro.interpreters

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.util.Timeout
import cats.effect.Sync
import org.chepiov.tomodoro.algebras.User
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.typeclasses.FromFuture

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UserInterpreter[F[_]: Sync: FromFuture](userActor: ActorRef)(implicit system: ActorSystem) extends User[F] {
  implicit val timeout: Timeout     = Timeout(5.seconds)
  implicit val ec: ExecutionContext = system.dispatcher

  override def advance(cmd: UserCommand): F[Unit] =
    FromFuture[F].fromFuture(Sync[F].delay((userActor ? cmd).map(_ => ())))
  override def info(query: UserInfoQuery): F[Unit] =
    FromFuture[F].fromFuture(Sync[F].delay((userActor ? query).map(_ => ())))
}

case object UserInterpreter {
  def apply[F[_]: Sync: FromFuture](userActor: ActorRef)(implicit system: ActorSystem): F[User[F]] =
    Sync[F].delay(new UserInterpreter(userActor))
}
