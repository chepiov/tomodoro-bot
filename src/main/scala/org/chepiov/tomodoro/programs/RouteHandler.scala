package org.chepiov.tomodoro.programs

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import cats.Monad
import cats.effect.Sync
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebra.Pomodoro
import org.chepiov.tomodoro.{BotUpdate, BotUser}

class RouteHandler[F[_]: Sync](pomodoro: Pomodoro[F], logger: Logger[F]) {

  def handleUpdate(update: BotUpdate): F[StatusCode] =
    for {
      _ <- logger.debug(s"Received update: $update")
      result <- if (update.message.isDefined)
                 pomodoro
                   .handleMessage(update.message.get)
                   .map[StatusCode](_ => StatusCodes.NoContent)
                   .handleErrorWith { e =>
                     for {
                       _ <- logger.error(e)("Error during handling update")
                     } yield StatusCodes.InternalServerError
                   } else Monad[F].pure[StatusCode](StatusCodes.NoContent)
    } yield result

  def handleInfo: F[BotUser] =
    for {
      _    <- logger.debug("Received bot info request")
      info <- pomodoro.getInfo
    } yield info
}

object RouteHandler {
  def apply[F[_]: Sync](pomodoro: Pomodoro[F]): F[RouteHandler[F]] =
    Sync[F].delay(new RouteHandler(pomodoro, Slf4jLogger.getLogger[F]))
}
