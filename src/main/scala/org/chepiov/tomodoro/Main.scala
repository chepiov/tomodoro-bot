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
import org.chepiov.tomodoro.http.resourceHttp
import org.chepiov.tomodoro.interpreters._
import pureconfig.generic.auto._
import pureconfig.module.catseffect._

import scala.concurrent.duration._

object Main extends IOApp {

  def actorSystem: IO[ActorSystem] = IO(ActorSystem("tomodoro"))

  def runServer(route: Route)(implicit system: ActorSystem, config: HttpConfig, logger: Logger[IO]): IO[Unit] =
    for {
      _ <- IO.fromFuture(IO {
            implicit val mat: ActorMaterializer = ActorMaterializer()
            Http().bindAndHandle(route, config.interface, config.port)
          })
      r <- logger.debug(s"Starting server on: ${config.interface}:${config.port}")
    } yield r

  def run(args: List[String]): IO[ExitCode] =
    for {
      implicit0(logger: Logger[IO])     <- Slf4jLogger.create[IO]
      implicit0(system: ActorSystem)    <- actorSystem
      implicit0(httpConfig: HttpConfig) <- loadConfigF[IO, HttpConfig]("http")
      telegramConfig                    <- loadConfigF[IO, TelegramConfig]("telegram")
      telegram                          <- TelegramInterpreter[IO](telegramConfig)
      userChat                          <- UserChatInterpreter[IO](telegram)
      users                             <- UsersInterpreter[IO](userChat, system)
      manager                           <- ManagerInterpreter[IO](users)
      tomodoro                          <- TomodoroInterpreter[IO](manager, telegram)
      updateRoute                       = resourceHttp.updateRoute("update", tomodoro)
      infoRoute                         = resourceHttp.infoRoute("info", tomodoro)
      _                                 <- runServer(updateRoute ~ infoRoute)
    } yield ExitCode.Success
}

final case class HttpConfig(interface: String = "0.0.0.0", port: Int = 8080, maxTimeout: FiniteDuration = 5.second)
