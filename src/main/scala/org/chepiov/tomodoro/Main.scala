package org.chepiov.tomodoro

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.functor._

object Main extends IOApp {
  import JsonSupport._

  def route(updates: Updates): Route =
    path("") {
      complete("Hello World")
    } ~
      pathPrefix("updates") {
        post {
          entity(as[BotUpdate]) { update =>
            complete(updates.add(update).as("").unsafeToFuture())
          }
        }
      }

  implicit val system: ActorSystem        = ActorSystem("tomodoro")
  implicit val materializer: Materializer = ActorMaterializer()

  def run(args: List[String]): IO[ExitCode] =
    for {
      updates  <- Updates(Nil)
      appRoute = route(updates)
      _        <- IO.fromFuture(IO(Http().bindAndHandle(appRoute, "0.0.0.0", 8080)))
    } yield ExitCode.Success
}

final class Updates(ref: Ref[IO, List[BotUpdate]]) {
  def allUpdates: IO[List[BotUpdate]]  = ref.get
  def add(update: BotUpdate): IO[Unit] = ref.update(update :: _)
}

case object Updates {
  def apply(initial: List[BotUpdate]): IO[Updates] =
    for (ref <- Ref[IO].of(initial)) yield new Updates(ref)
}
