package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.{ExitCode, IO, IOApp}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.api.Pomodoro
import org.chepiov.tomodoro.impl.{PomodoroImpl, TelegramImpl}
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

object Main extends IOApp {
  import JsonSupport._

  def route(pomodoro: Pomodoro[IO], logger: Logger[IO]): Route =
    path("") {
      complete(pomodoro.getInfo.unsafeToFuture())
    } ~
      pathPrefix("updates") {
        post {
          entity(as[BotUpdate]) { update =>
            complete(handleUpdate(pomodoro, update, logger).unsafeToFuture())
          }
        }
      }

  private def handleUpdate(pomodoro: Pomodoro[IO], update: BotUpdate, logger: Logger[IO]): IO[StatusCode] =
    for {
      _ <- logger.debug(s"Received update: $update")
      result <- if (update.message.isDefined)
                 pomodoro
                   .handleMessage(update.message.get)
                   .map(_ => StatusCodes.NoContent)
                   .handleErrorWith { e =>
                     for {
                       _ <- logger.error(e)("Error during handling update")
                     } yield StatusCodes.InternalServerError
                   } else IO.pure(StatusCodes.NoContent)
    } yield result

  implicit val system: ActorSystem        = ActorSystem("tomodoro")
  implicit val materializer: Materializer = ActorMaterializer()

  def run(args: List[String]): IO[ExitCode] =
    for {
      telegramConfig <- loadConfigF[IO, TelegramConfig]("telegram")
      httpConfig     <- loadConfigF[IO, HttpConfig]("http")
      logger         = Slf4jLogger.getLogger[IO]
      telegram       <- TelegramImpl[IO](telegramConfig, logger)
      pomodoro       <- PomodoroImpl[IO](telegram, logger)
      appRoute       = route(pomodoro, logger)
      _              <- IO.fromFuture(IO(Http().bindAndHandle(appRoute, httpConfig.interface, httpConfig.port)))
    } yield ExitCode.Success
}
