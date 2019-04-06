package org.chepiov.tomodoro.typeclasses

import cats.effect.IO
import simulacrum.typeclass

import scala.concurrent.Future

@typeclass
trait ToFuture[F[_]] {
  def toFuture[A](fa: F[A]): Future[A]
}

case object ToFuture {
  implicit val ioToFuture: ToFuture[IO] = new ToFuture[IO] {
    override def toFuture[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
  }
}
