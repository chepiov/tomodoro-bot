package org.chepiov.tomodoro

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class BotUpdate(updateId: Long, message: Option[BotMessage])
final case class BotMessage(messageId: Long, chat: BotChat, text: Option[String])
final case class BotChat(id: Long)
final case class BotUser(
    id: Long,
    isBot: Boolean,
    firstName: String,
    lastName: Option[String],
    userName: Option[String],
    languageCode: Option[String]
)
final case class BotResponse[A](ok: Boolean, result: A)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val chatFormat: RootJsonFormat[BotChat]       = jsonFormat1(BotChat)
  implicit val messageFormat: RootJsonFormat[BotMessage] = jsonFormat(BotMessage.apply, "message_id", "chat", "text")
  implicit val updateFormat: RootJsonFormat[BotUpdate]   = jsonFormat(BotUpdate.apply, "update_id", "message")
  implicit val userFormat: RootJsonFormat[BotUser] =
    jsonFormat(BotUser.apply, "id", "is_bot", "first_name", "last_name", "username", "language_code")
  implicit def responseFormat[A: RootJsonFormat]: RootJsonFormat[BotResponse[A]] = jsonFormat2(BotResponse.apply[A])

}

object JsonSupport extends JsonSupport

final case class TelegramConfig(token: String, host: String = "api.telegram.org", scheme: String = "https")
final case class HttpConfig(interface: String = "0.0.0.0", port: Int = 8080)
