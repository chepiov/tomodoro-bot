package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import org.chepiov.tomodoro.algebras.Telegram.TUpdate
import org.chepiov.tomodoro.algebras.WebHook
import org.chepiov.tomodoro.interpreters._
import org.chepiov.tomodoro.programs.WebHookProgram
import org.chepiov.tomodoro.typeclasses.ToFuture
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

object Main extends IOApp {

  def route[F[_]: ToFuture: Sync](webHook: WebHook[F]): Route = {
    import JsonSupport._
    import WebHookProgram._

    pathPrefix("updates") {
      post {
        entity(as[TUpdate]) { updateMessage =>
          complete(update(updateMessage)(webHook))
        }
      }
    }
  }
  implicit val system: ActorSystem        = ActorSystem("tomodoro")
  implicit val materializer: Materializer = ActorMaterializer()

  def run(args: List[String]): IO[ExitCode] =
    for {
      mainLogger      <- LoggerInterpreter[IO]("main")
      telegramLogger  <- LoggerInterpreter[IO]("telegram")
      tomodoroLogger  <- LoggerInterpreter[IO]("tomodoro")
      messengerLogger <- LoggerInterpreter[IO]("messenger")
      httpConfig      <- loadConfigF[IO, HttpConfig]("http")
      telegramConfig  <- loadConfigF[IO, TelegramConfig]("telegram")
      telegram        <- TelegramInterpreter[IO](telegramConfig, telegramLogger)
      messenger       <- MessengerInterpreter[IO](telegram, messengerLogger)
      users           <- UsersInterpreter[IO](messenger)
      tomodoro        <- TomodoroInterpreter[IO](users, tomodoroLogger)
      webHookLogger   <- LoggerInterpreter[IO]("web hook")
      webHook         <- WebHookInterpreter[IO](tomodoro, webHookLogger)
      _               <- mainLogger.debug(s"Starting server on: ${httpConfig.interface}:${httpConfig.port}")
      appRoute        <- IO(route(webHook))
      _               <- IO.fromFuture(IO(Http().bindAndHandle(appRoute, httpConfig.interface, httpConfig.port)))
    } yield ExitCode.Success
}

final case class HttpConfig(interface: String = "0.0.0.0", port: Int = 8080)
