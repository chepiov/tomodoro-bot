package org.chepiov.tomodoro.algebras

import cats.effect.IO
import simulacrum.typeclass

import scala.concurrent.Future

/**
  * Represents converter from effect to Future
  *
  * @tparam F effect
  */
@typeclass
trait ToFuture[F[_]] {

  /**
    * Converts effect to Future
    *
    * @param fa to convert
    * @tparam A effective value
    * @return future
    */
  def toFuture[A](fa: F[A]): Future[A]
}

case object ToFuture {
  implicit val ioToFuture: ToFuture[IO] = new ToFuture[IO] {
    override def toFuture[A](fa: IO[A]): Future[A] = fa.unsafeToFuture()
  }
}
