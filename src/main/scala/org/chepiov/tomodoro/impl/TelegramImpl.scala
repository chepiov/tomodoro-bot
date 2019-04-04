package org.chepiov.tomodoro.impl

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, Uri}
import cats.effect.{IO, LiftIO, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.chepiov.tomodoro.TelegramConfig
import org.chepiov.tomodoro.api.Telegram

class TelegramImpl[F[_]: Sync: LiftIO](config: TelegramConfig)(implicit actorSystem: ActorSystem) extends Telegram[F] {
  private val uri = s"${config.scheme}://${config.host}/bot${config.token}/sendMessage"

  def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long]): F[Unit] = {
    for {
      req <- request(chatId, text, messageIdReplyTo)
      _   <- LiftIO[F].liftIO(IO.fromFuture(IO(Http().singleRequest(req))))
    } yield ()
  }

  def request(chatId: Long, text: String, messageIdReplyTo: Option[Long]): F[HttpRequest] =
    Sync[F].delay {
      val query = Query {
        Map("chat_id" -> chatId.toString, "text" -> text, "parse_mode" -> "Markdown") ++
          messageIdReplyTo.map(id => Map("reply_to_message_id" -> id.toString)).getOrElse(Map())
      }
      HttpRequest(method = HttpMethods.GET, uri = Uri(uri).withQuery(query))
    }
}

object TelegramImpl {
  def apply[F[_]: Sync: LiftIO](config: TelegramConfig)(implicit actorSystem: ActorSystem): F[TelegramImpl[F]] =
    Sync[F].delay(new TelegramImpl(config))
}
