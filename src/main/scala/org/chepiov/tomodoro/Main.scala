package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.{ExitCode, IO, IOApp}
import org.chepiov.tomodoro.api.Pomodoro
import org.chepiov.tomodoro.impl.{PomodoroImpl, TelegramImpl}
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

object Main extends IOApp {
  import JsonSupport._

  def route(pomodoro: Pomodoro[IO]): Route =
    path("") {
      complete(pomodoro.getInfo.unsafeToFuture())
    } ~
      pathPrefix("updates") {
        post {
          entity(as[BotUpdate]) { update =>
            update.message.foreach { message =>
              pomodoro.handleMessage(message)
            }
            complete(StatusCodes.NoContent)
          }
        }
      }

  implicit val system: ActorSystem        = ActorSystem("tomodoro")
  implicit val materializer: Materializer = ActorMaterializer()

  def run(args: List[String]): IO[ExitCode] =
    for {
      telegramConfig <- loadConfigF[IO, TelegramConfig]("telegram")
      httpConfig     <- loadConfigF[IO, HttpConfig]("http")
      botApi         <- TelegramImpl[IO](telegramConfig)
      pomodoroApi    = new PomodoroImpl[IO](botApi)
      appRoute       = route(pomodoroApi)
      _              <- IO.fromFuture(IO(Http().bindAndHandle(appRoute, httpConfig.interface, httpConfig.port)))
    } yield ExitCode.Success
}
