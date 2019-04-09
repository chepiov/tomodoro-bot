package org.chepiov.tomodoro.interpreters

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import org.chepiov.tomodoro.algebra.Telegram._
import org.chepiov.tomodoro.algebra.{Logger, Telegram}
import org.chepiov.tomodoro.typeclasses.FromFuture

class TelegramInterpreter[F[_]: Sync: FromFuture](config: TelegramConfig, logger: Logger[F])(
    implicit actorSystem: ActorSystem
) extends Telegram[F] {
  import org.chepiov.tomodoro.JsonSupport._

  implicit private val materializer: Materializer = ActorMaterializer()

  private val uri = s"${config.scheme}://${config.host}/bot${config.token}"

  override def help(chatId: Long, messageIdReplyTo: Option[Long] = None): F[Unit] =
    sendMessage(chatId, helpMessage, messageIdReplyTo)

  override def run(chatId: Long): F[Unit] =
    sendMessage(chatId, ranMessage)

  override def end(chatId: Long): F[Unit] =
    sendMessage(chatId, endedMessage)

  override def settings(chatId: Long, settings: Settings, messageIdReplyTo: Option[Long] = None): F[Unit] =
    sendMessage(chatId, settingsMessage(settings), messageIdReplyTo)

  override def custom(chatId: Long, message: String, messageIdReplyTo: Option[Long] = None): F[Unit] =
    sendMessage(chatId, message, messageIdReplyTo)

  override def me(): F[TUser] =
    for {
      _   <- logger.debug(s"Fetching info about me")
      req <- Sync[F].delay(HttpRequest(method = HttpMethods.GET, uri = Uri(uri + "/getMe")))
      res <- FromFuture[F].fromFuture(Sync[F].delay(Http().singleRequest(req)))
      _   <- logger.debug(s"Fetch result: $res")
      me  <- FromFuture[F].fromFuture(Sync[F].delay(Unmarshal(res).to[TResponse[TUser]]))
    } yield me.result

  private def createRequest(chatId: Long, text: String, messageIdReplyTo: Option[Long]): F[HttpRequest] =
    Sync[F].delay {
      val query = Query {
        Map("chat_id" -> chatId.toString, "text" -> text, "parse_mode" -> "Markdown") ++
          messageIdReplyTo.map(id => Map("reply_to_message_id" -> id.toString)).getOrElse(Map())
      }
      HttpRequest(method = HttpMethods.GET, uri = Uri(uri + "/sendMessage").withQuery(query))
    }

  def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long] = None): F[Unit] =
    for {
      request  <- createRequest(chatId, text, messageIdReplyTo)
      _        <- logger.debug(s"[$chatId] Sending message: $request")
      response <- FromFuture[F].fromFuture(Sync[F].delay(Http().singleRequest(request)))
      _        <- logger.debug(s"[$chatId] Send message result: $response")
      _        <- Sync[F].delay(response.discardEntityBytes())
    } yield ()
}

object TelegramInterpreter {
  def apply[F[_]: Sync: FromFuture](config: TelegramConfig, logger: Logger[F])(
      implicit actorSystem: ActorSystem
  ): Telegram[F] =
    new TelegramInterpreter[F](config, logger)
}
final case class TelegramConfig(token: String, host: String = "api.telegram.org", scheme: String = "https")
