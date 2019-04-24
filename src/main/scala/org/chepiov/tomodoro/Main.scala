package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import cats.effect.{ExitCode, IO, IOApp}
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Telegram.TelegramConfig
import org.chepiov.tomodoro.http.RouteHandler
import org.chepiov.tomodoro.interpreters.RepositoryInterpreter.DbConfig
import org.chepiov.tomodoro.interpreters._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

import scala.concurrent.duration._

object Main extends IOApp {

  def actorSystem: IO[ActorSystem] = IO(ActorSystem("tomodoro"))

  def runServer(route: Route, config: HttpConfig, logger: Logger[IO])(implicit system: ActorSystem): IO[Unit] =
    for {
      _ <- logger.debug("Initializing server")
      _ <- IO.fromFuture(IO {
            implicit val mat: ActorMaterializer = ActorMaterializer()
            Http().bindAndHandle(route, config.interface, config.port)
          })
      r <- logger.debug(s"Starting server on: ${config.interface}:${config.port}")
    } yield r

  def run(args: List[String]): IO[ExitCode] =
    for {
      implicit0(system: ActorSystem) <- actorSystem
      telegramConfig                 <- loadConfigF[IO, TelegramConfig]("telegram")
      telegram                       <- TelegramInterpreter[IO](telegramConfig)
      dbConfig                       <- loadConfigF[IO, DbConfig]("database")
      repository                     <- RepositoryInterpreter[IO](dbConfig)
      users                          <- UsersInterpreter[IO](telegram, repository, system)
      statistic                      <- StatisticInterpreter[IO](repository)
      tomodoro                       <- TomodoroInterpreter[IO](users, statistic, telegram)
      httpConfig                     <- loadConfigF[IO, HttpConfig]("http")
      logger                         <- Slf4jLogger.create[IO]
      routeHandler                   = new RouteHandler[IO](httpConfig, logger)
      updateRoute                    = routeHandler.updateRoute(s"${telegramConfig.token}", tomodoro)
      infoRoute                      = routeHandler.infoRoute("info", tomodoro)
      _                              <- runServer(updateRoute ~ infoRoute, httpConfig, logger)
    } yield ExitCode.Success
}

final case class HttpConfig(interface: String = "0.0.0.0", port: Int = 8080, maxTimeout: FiniteDuration = 5.second)
