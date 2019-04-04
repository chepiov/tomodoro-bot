package org.chepiov.tomodoro

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class BotUpdate(updateId: Long, message: Option[BotMessage])
final case class BotMessage(messageId: Long, chat: BotChat, text: Option[String])
final case class BotChat(id: Long)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val chatFormat: RootJsonFormat[BotChat]       = jsonFormat1(BotChat)
  implicit val messageFormat: RootJsonFormat[BotMessage] = jsonFormat(BotMessage.apply, "message_id", "chat", "text")
  implicit val updateFormat: RootJsonFormat[BotUpdate]   = jsonFormat(BotUpdate.apply, "update_id", "message")
}

object JsonSupport extends JsonSupport

final case class TelegramConfig(token: String, host: String = "api.telegram.org", scheme: String = "https")
final case class HttpConfig(interface: String = "0.0.0.0", port: Int = 8080)
