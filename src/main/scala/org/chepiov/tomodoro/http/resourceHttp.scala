package org.chepiov.tomodoro.http

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.effect.{ContextShift, IO, Timer}
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.HttpConfig
import org.chepiov.tomodoro.algebras.Telegram.TUpdate
import org.chepiov.tomodoro.algebras.Tomodoro
import spray.json.DefaultJsonProtocol._

object resourceHttp {
  def updateRoute(
      prefix: String,
      tomodoro: Tomodoro[IO]
  )(implicit cs: ContextShift[IO], timer: Timer[IO], logger: Logger[IO], config: HttpConfig): Route =
    pathPrefix(prefix) {
      post {
        entity(as[TUpdate]) { update =>
          complete(tomodoro.handleUpdate(update))
        }
      }
    }

  def infoRoute(
      prefix: String,
      tomodoro: Tomodoro[IO]
  )(implicit cs: ContextShift[IO], timer: Timer[IO], logger: Logger[IO], config: HttpConfig): Route =
    pathPrefix(prefix) {
      get {
        complete(tomodoro.getInfo)
      }
    }
}
