package org.chepiov.tomodoro.programs

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import cats.syntax.functor._
import org.chepiov.tomodoro.algebras.Telegram.TUpdate
import org.chepiov.tomodoro.algebras.WebHook
import org.chepiov.tomodoro.interpreters.MError
import org.chepiov.tomodoro.typeclasses.ToFuture
import org.chepiov.tomodoro.typeclasses.ToFuture.ops._

import scala.concurrent.Future

/**
  * Web hook depended operations program.
  */
object WebHookProgram {

  def update[F[_]: ToFuture: MError](update: TUpdate)(webHook: WebHook[F]): Future[StatusCode] = {
    webHook
      .handleUpdate(update)
      .map[StatusCode](r => if (r) StatusCodes.NoContent else StatusCodes.InternalServerError)
      .toFuture
  }
}
