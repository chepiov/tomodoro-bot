package org.chepiov.tomodoro.interpreter

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpMethods, HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.IO
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebra.Telegram
import org.chepiov.tomodoro.{BotResponse, BotUser, TelegramConfig}

class TelegramInterpreter(config: TelegramConfig, logger: Logger[IO])(implicit actorSystem: ActorSystem)
    extends Telegram[IO] {
  import org.chepiov.tomodoro.JsonSupport._

  implicit private val materializer: Materializer = ActorMaterializer()

  private val uri = s"${config.scheme}://${config.host}/bot${config.token}"

  override def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long]): IO[Unit] = {
    for {
      req <- sendMessageRequest(chatId, text, messageIdReplyTo)
      res <- IO.fromFuture(IO(Http().singleRequest(req)))
      _   <- logger.debug(s"[sendMessage] Received response: $res")
      _   <- IO(res.discardEntityBytes())
    } yield ()
  }

  override def me(): IO[BotUser] =
    for {
      req <- IO(HttpRequest(method = HttpMethods.GET, uri = Uri(uri + "/getMe")))
      res <- IO.fromFuture(IO(Http().singleRequest(req)))
      _   <- logger.debug(s"[me] Received response: $res")
      me  <- IO.fromFuture(IO(Unmarshal(res).to[BotResponse[BotUser]]))
    } yield me.result

  private def sendMessageRequest(chatId: Long, text: String, messageIdReplyTo: Option[Long]): IO[HttpRequest] =
    IO {
      val query = Query {
        Map("chat_id" -> chatId.toString, "text" -> text, "parse_mode" -> "Markdown") ++
          messageIdReplyTo.map(id => Map("reply_to_message_id" -> id.toString)).getOrElse(Map())
      }
      HttpRequest(method = HttpMethods.GET, uri = Uri(uri + "/sendMessage").withQuery(query))
    }
}

object TelegramInterpreter {
  def apply(config: TelegramConfig)(implicit actorSystem: ActorSystem): Telegram[IO] =
    new TelegramInterpreter(config, Slf4jLogger.getLogger[IO])
}
