package org.chepiov.tomodoro.algebras

/**
  * Isomorphism between types A and B.
  *
  * @tparam A from
  * @tparam B to
  */
trait Iso[A, B] {

  def wrap(a: A): B

  def unwrap(b: B): A
}

case object Iso {

  def apply[A, B](implicit iso: Iso[A, B]): Iso[A, B] = iso

  object syntax {
    implicit class IsoOps[A](value: A) {

      def wrap[B](implicit iso: Iso[A, B]): B = iso.wrap(value)

      def unwrap[B](implicit iso: Iso[B, A]): B = iso.unwrap(value)
    }
  }

  /**
    * Wrapper for any type T.
    *
    * @tparam T value
    */
  trait Wrapper[T] extends Any {
    def value: T
  }

  trait WrapperCompanion[F[x] <: Wrapper[x]] {

    def apply[T](x: T): F[T]

    implicit def toIso[T]: Iso[T, F[T]] = new Iso[T, F[T]] {
      def wrap(a: T): F[T] = apply(a)

      def unwrap(b: F[T]): T = b.value
    }
  }
}
