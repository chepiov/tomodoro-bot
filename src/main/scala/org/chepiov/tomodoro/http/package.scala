package org.chepiov.tomodoro

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, MediaTypes}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import cats.effect.{ContextShift, IO, Timer}
import cats.syntax.applicative._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram.{TChat, TMessage, TUpdate, TUser}
import spray.json.DefaultJsonProtocol._
import spray.json.{RootJsonFormat, _}

package object http {
  implicit def toResponseMarshaller[A: JsonWriter](
      implicit cs: ContextShift[IO],
      timer: Timer[IO],
      logger: Logger[IO],
      config: HttpConfig
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

  implicit val chatFormat: RootJsonFormat[TChat]       = jsonFormat1(TChat)
  implicit val messageFormat: RootJsonFormat[TMessage] = jsonFormat(TMessage.apply, "message_id", "chat", "text")
  implicit val updateFormat: RootJsonFormat[TUpdate]   = jsonFormat(TUpdate.apply, "update_id", "message")
  implicit val userFormat: RootJsonFormat[TUser] =
    jsonFormat(TUser.apply, "id", "is_bot", "first_name", "last_name", "username")
}
