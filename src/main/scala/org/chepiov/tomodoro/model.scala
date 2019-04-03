package org.chepiov.tomodoro

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

final case class BotUpdate(update_id: Long, message: Option[BotMessage])
final case class BotMessage(message_id: Long, chat: BotChat, text: Option[String])
final case class BotChat(id: Long)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val chatFormat: RootJsonFormat[BotChat]       = jsonFormat1(BotChat)
  implicit val updateFormat: RootJsonFormat[BotUpdate]   = jsonFormat2(BotUpdate)
  implicit val messageFormat: RootJsonFormat[BotMessage] = jsonFormat3(BotMessage)
}

object JsonSupport extends JsonSupport
