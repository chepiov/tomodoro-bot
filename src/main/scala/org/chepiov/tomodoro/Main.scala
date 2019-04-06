package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.{ExitCode, IO, IOApp, Sync}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.JsonSupport._
import org.chepiov.tomodoro.interpreter.{PomodoroInterpreter, TelegramInterpreter}
import org.chepiov.tomodoro.programs.RouteHandler
import org.chepiov.tomodoro.typeclasses.ToFuture
import org.chepiov.tomodoro.typeclasses.ToFuture.ops._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

object Main extends IOApp {

  def route[F[_]: Sync: ToFuture](routeHandler: RouteHandler[F]): Route = {
    import routeHandler._

    path("") {
      complete(handleInfo.toFuture)
    } ~
      pathPrefix("updates") {
        post {
          entity(as[BotUpdate]) { update =>
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
      telegram       = TelegramInterpreter(telegramConfig)
      pomodoro       <- PomodoroInterpreter[IO](telegram)
      routeHandler   <- RouteHandler(pomodoro)
      logger         = Slf4jLogger.getLogger[IO]
      _              <- logger.debug(s"Starting server on: ${httpConfig.interface}:${httpConfig.port}")
      appRoute       = route(routeHandler)
      _              <- IO.fromFuture(IO(Http().bindAndHandle(appRoute, httpConfig.interface, httpConfig.port)))
    } yield ExitCode.Success
}
