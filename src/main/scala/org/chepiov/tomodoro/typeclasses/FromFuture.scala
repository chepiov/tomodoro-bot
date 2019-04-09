package org.chepiov.tomodoro.typeclasses

import cats.effect.IO

import scala.concurrent.Future

trait FromFuture[F[_]] {
  def fromFuture[A](a: F[Future[A]]): F[A]
}

case object FromFuture {

  def apply[F[_]](implicit F: FromFuture[F]): FromFuture[F] = F

  implicit val ioFromFuture: FromFuture[IO] = new FromFuture[IO] {
    def fromFuture[A](a: IO[Future[A]]): IO[A] = IO.fromFuture(a)
  }
}
