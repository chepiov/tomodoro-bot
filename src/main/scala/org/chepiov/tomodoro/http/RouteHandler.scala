package org.chepiov.tomodoro.http

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.applicative._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.HttpConfig
import org.chepiov.tomodoro.algebras.Telegram.TUpdate
import org.chepiov.tomodoro.algebras.Tomodoro
import spray.json.DefaultJsonProtocol._
import spray.json.{JsonWriter, _}

class RouteHandler(config: HttpConfig, logger: Logger[IO])(implicit cs: ContextShift[IO], timer: Timer[IO]) {

  def updateRoute(prefix: String, tomodoro: Tomodoro[IO]): Route = {
    pathPrefix(prefix) {
      post {
        entity(as[TUpdate]) { update =>
          complete(tomodoro.handleUpdate(update))
        }
      }
    }
  }

  def infoRoute(prefix: String, tomodoro: Tomodoro[IO]): Route = {
    pathPrefix(prefix) {
      post {
        entity(as[TUpdate]) { update =>
          complete(tomodoro.handleUpdate(update))
        }
      }
    }
  }

  implicit def toResponseMarshaller[A: JsonWriter](
      implicit cs: ContextShift[IO],
      timer: Timer[IO]
  ): ToEntityMarshaller[IO[A]] =
    Marshaller.withFixedContentType(MediaTypes.`application/json`) { ia: IO[A] =>
      val iaWithTimeout =
        IO.race(
            timer.sleep(config.maxTimeout),
            ia.map(a => ByteString(a.toJson.compactPrint))
          )
          .flatMap {
            case Right(s) => s.pure[IO]
            case Left(()) => IO.raiseError(new RuntimeException("Timeout"))
          }

      val result = iaWithTimeout.handleErrorWith { e =>
        for {
          _ <- logger.error(e)("Error during handling request")
          a <- IO.raiseError[ByteString](e)
        } yield a
      }

      HttpEntity(
        ContentTypes.`application/json`,
        Source.fromFuture(result.unsafeToFuture())
      )
    }
}
