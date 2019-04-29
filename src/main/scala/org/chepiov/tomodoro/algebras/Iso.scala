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

  def apply[A, B](implicit F: Iso[A, B]): Iso[A, B] = F

  object syntax {

    implicit class IsoOps[A](value: A) {
      def wrap[B](implicit F: Iso[A, B]): B = F.wrap(value)

      def unwrap[B](implicit F: Iso[B, A]): B = F.unwrap(value)
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

    implicit def wrapperIso[T]: Iso[T, F[T]] = new Iso[T, F[T]] {
      def wrap(a: T): F[T] = apply(a)

      def unwrap(b: F[T]): T = b.value
    }
  }

}
