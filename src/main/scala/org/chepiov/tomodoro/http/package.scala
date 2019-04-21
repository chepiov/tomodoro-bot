package org.chepiov.tomodoro

import org.chepiov.tomodoro.algebras.Telegram._
import spray.json.DefaultJsonProtocol._
import spray.json.RootJsonFormat

package object http {
  implicit val chatFormat: RootJsonFormat[TChat]       = jsonFormat1(TChat)
  implicit val messageFormat: RootJsonFormat[TMessage] = jsonFormat(TMessage.apply, "message_id", "chat", "text")
  implicit val userFormat: RootJsonFormat[TUser] =
    jsonFormat(TUser.apply, "id", "is_bot", "first_name", "last_name", "username")
  implicit val callbackQueryFormat: RootJsonFormat[TCallbackQuery] = jsonFormat4(TCallbackQuery)
  implicit val updateFormat: RootJsonFormat[TUpdate] =
    jsonFormat(TUpdate.apply, "update_id", "message", "callback_query")
}
