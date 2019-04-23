package org.chepiov.tomodoro.http

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.effect._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.HttpConfig
import org.chepiov.tomodoro.algebras.Telegram.TUpdate
import org.chepiov.tomodoro.algebras.Tomodoro
import spray.json.DefaultJsonProtocol._
import spray.json.{JsonWriter, _}

class RouteHandler[F[_]: ConcurrentEffect](config: HttpConfig, logger: Logger[F])(
    implicit timer: Timer[F]
) {

  def updateRoute(prefix: String, tomodoro: Tomodoro[F]): Route = {
    pathPrefix(prefix) {
      post {
        entity(as[TUpdate]) { update =>
          complete(tomodoro.handleUpdate(update))
        }
      }
    }
  }

  def infoRoute(prefix: String, tomodoro: Tomodoro[F]): Route =
    pathPrefix(prefix) {
      get {
        complete(tomodoro.getInfo)
      }
    }

  implicit def toResponseMarshaller[A: JsonWriter](
      implicit timer: Timer[F]
  ): ToEntityMarshaller[F[A]] =
    Marshaller.withFixedContentType(MediaTypes.`application/json`) { ia: F[A] =>
      val iaWithTimeout =
        Concurrent[F]
          .race(
            timer.sleep(config.maxTimeout),
            ia.map(a => ByteString(a.toJson.compactPrint))
          )
          .flatMap {
            case Right(s) => s.pure[F]
            case Left(()) => new RuntimeException("Timeout").raiseError[F, ByteString]
          }

      val result = iaWithTimeout.handleErrorWith { e =>
        for {
          _ <- logger.error(e)("Error during handling request")
          a <- e.raiseError[F, ByteString]
        } yield a
      }

      HttpEntity(
        ContentTypes.`application/json`,
        Source.fromFuture(Effect[F].toIO(result).unsafeToFuture())
      )
    }
}
