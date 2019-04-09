package org.chepiov.tomodoro

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import org.chepiov.tomodoro.algebra.Telegram._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val chatFormat: RootJsonFormat[TChat]       = jsonFormat1(TChat)
  implicit val messageFormat: RootJsonFormat[TMessage] = jsonFormat(TMessage.apply, "message_id", "chat", "text")
  implicit val updateFormat: RootJsonFormat[TUpdate]   = jsonFormat(TUpdate.apply, "update_id", "message")
  implicit val userFormat: RootJsonFormat[TUser] =
    jsonFormat(TUser.apply, "id", "is_bot", "first_name", "last_name", "username", "language_code")
  implicit def responseFormat[A: RootJsonFormat]: RootJsonFormat[TResponse[A]] = jsonFormat2(TResponse.apply[A])
}

object JsonSupport extends JsonSupport