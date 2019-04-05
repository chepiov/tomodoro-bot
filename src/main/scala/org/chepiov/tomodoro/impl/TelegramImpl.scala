package org.chepiov.tomodoro.impl

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.{IO, LiftIO, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.chepiov.tomodoro.api.Telegram
import org.chepiov.tomodoro.{BotResponse, BotUser, TelegramConfig}

class TelegramImpl[F[_]: Sync: LiftIO](config: TelegramConfig)(implicit actorSystem: ActorSystem) extends Telegram[F] {
  import org.chepiov.tomodoro.JsonSupport._

  implicit private val materializer: Materializer = ActorMaterializer()

  private val uri = s"${config.scheme}://${config.host}/bot${config.token}"

  def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long]): F[Unit] = {
    for {
      req <- sendMessageRequest(chatId, text, messageIdReplyTo)
      _   <- LiftIO[F].liftIO(IO.fromFuture(IO(Http().singleRequest(req))))
    } yield ()
  }

  private def sendMessageRequest(chatId: Long, text: String, messageIdReplyTo: Option[Long]): F[HttpRequest] =
    Sync[F].delay {
      val query = Query {
        Map("chat_id" -> chatId.toString, "text" -> text, "parse_mode" -> "Markdown") ++
          messageIdReplyTo.map(id => Map("reply_to_message_id" -> id.toString)).getOrElse(Map())
      }
      HttpRequest(method = HttpMethods.GET, uri = Uri(uri + "/sendMessage").withQuery(query))
    }

  def getMe: F[BotUser] =
    for {
      req <- Sync[F].delay(HttpRequest(method = HttpMethods.GET, uri = Uri(uri + "/getMe")))
      res <- LiftIO[F].liftIO(IO.fromFuture(IO(Http().singleRequest(req))))
      _   <- Sync[F].delay(println(res))
      me  <- LiftIO[F].liftIO(IO.fromFuture(IO(Unmarshal(res).to[BotResponse[BotUser]])))
    } yield me.result
}

object TelegramImpl {
  def apply[F[_]: Sync: LiftIO](config: TelegramConfig)(implicit actorSystem: ActorSystem): F[TelegramImpl[F]] =
    Sync[F].delay(new TelegramImpl(config))
}
