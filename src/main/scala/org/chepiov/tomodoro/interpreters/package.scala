package org.chepiov.tomodoro

import cats.MonadError

package object interpreters {
  type MError[F[_]] = MonadError[F, Throwable]
}
