package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import org.chepiov.tomodoro.algebra.Telegram.TUpdate
import org.chepiov.tomodoro.interpreters.{LoggerInterpreter, PomodoroInterpreter, TelegramConfig, TelegramInterpreter}
import org.chepiov.tomodoro.programs.RouteHandler
import org.chepiov.tomodoro.typeclasses.ToFuture
import org.chepiov.tomodoro.typeclasses.ToFuture.ops._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

object Main extends IOApp {

  def route[F[_]: Sync: ToFuture](routeHandler: RouteHandler[F]): Route = {
    import JsonSupport._
    import routeHandler._

    path("") {
      complete(handleInfo.toFuture)
    } ~
      pathPrefix("updates") {
        post {
          entity(as[TUpdate]) { update =>
            complete(handleUpdate(update).toFuture)
          }
        }
      }
  }
  implicit val system: ActorSystem        = ActorSystem("tomodoro")
  implicit val materializer: Materializer = ActorMaterializer()

  def run(args: List[String]): IO[ExitCode] =
    for {
      telegramConfig <- loadConfigF[IO, TelegramConfig]("telegram")
      httpConfig     <- loadConfigF[IO, HttpConfig]("http")
      telegram       = TelegramInterpreter(telegramConfig, new LoggerInterpreter[IO]("telegram"))
      pomodoro       <- PomodoroInterpreter[IO](telegram, new LoggerInterpreter[IO]("pomodoro"))
      routeHandler   <- RouteHandler(pomodoro, new LoggerInterpreter[IO]("route"))
      logger         = new LoggerInterpreter[IO]("main")
      _              <- logger.debug(s"Starting server on: ${httpConfig.interface}:${httpConfig.port}")
      appRoute       = route(routeHandler)
      _              <- IO.fromFuture(IO(Http().bindAndHandle(appRoute, httpConfig.interface, httpConfig.port)))
    } yield ExitCode.Success
}

final case class HttpConfig(interface: String = "0.0.0.0", port: Int = 8080)
