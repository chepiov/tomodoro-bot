package org.chepiov.tomodoro.algebras

import simulacrum.typeclass

/**
  * Represents Telegram API.
  *
  * @see [[https://core.telegram.org/bots/api]]
  * @tparam F effect
  */
@typeclass
trait Telegram[F[_]] {

  import Telegram._

  /**
    * sendMessage method.
    *
    * @param message to send
    * @see [[https://core.telegram.org/bots/api#sendmessage]]
    */
  def sendMessage(message: TSendMessage): F[Unit]

  /**
    * answerCallbackQuery method.
    *
    * @param answer to send
    * @see [[https://core.telegram.org/bots/api#answercallbackquery]]
    */
  def answerCallbackQuery(answer: TCallbackAnswer): F[Unit]

  /**
    * editMessageText method.
    *
    * @param message to send
    * @see [[https://core.telegram.org/bots/api#editmessagetext]]
    */
  def editMessageText(message: TEditMessage): F[Unit]

  /**
    * getMe method.
    *
    * @return bot information
    * @see https://core.telegram.org/bots/api#getme
    */
  def getMe: F[TUser]
}

case object Telegram {

  /**
    * Telegram configuration.
    *
    * @param token  Telegram API token
    * @param host   Telegram API host
    * @param scheme Telegram API scheme
    */
  final case class TelegramConfig(token: String, host: String, scheme: String)

  /**
    * @see [[https://core.telegram.org/bots/api#user]].
    */
  final case class TUser(
      id: Long,
      isBot: Boolean,
      firstName: String,
      lastName: Option[String],
      username: Option[String]
  )

  /**
    * @see [[https://core.telegram.org/bots/api#chat]].
    */
  final case class TChat(id: Long)

  /**
    * @see [[https://core.telegram.org/bots/api#message]].
    */
  final case class TMessage(
      messageId: Long,
      chat: TChat,
      text: Option[String]
  )

  /**
    * @see [[https://core.telegram.org/bots/api#getting-updates]].
    */
  final case class TUpdate(
      updateId: Long,
      message: Option[TMessage],
      callbackQuery: Option[TCallbackQuery]
  )

  /**
    * @see [[https://core.telegram.org/bots/api#sendmessage]] reply_markup field.
    */
  sealed trait TReplyMarkup extends Serializable with Product

  /**
    * @see [[https://core.telegram.org/bots/api#sendmessage]].
    */
  final case class TSendMessage(
      chatId: Long,
      text: String,
      replyMarkup: Option[TReplyMarkup] = None,
      parseMode: String = "Markdown"
  )

  /**
    * @see [[https://core.telegram.org/bots/api#keyboardbutton]]
    */
  final case class TKeyboardButton(text: String)

  /**
    * @see [[https://core.telegram.org/bots/api#inlinekeyboardbutton]]
    */
  final case class TInlineKeyboardButton(text: String, callbackData: String)

  /**
    * @see [[https://core.telegram.org/bots/api#replykeyboardmarkup]]
    */
  final case class TReplyKeyboardMarkup(keyboard: List[List[TKeyboardButton]]) extends TReplyMarkup

  /**
    * @see [[https://core.telegram.org/bots/api#inlinekeyboardmarkup]]
    */
  final case class TInlineKeyboardMarkup(inlineKeyboard: List[List[TInlineKeyboardButton]]) extends TReplyMarkup

  /**
    * @see [[https://core.telegram.org/bots/api#callbackquery]]
    */
  final case class TCallbackQuery(id: String, from: TUser, message: Option[TMessage], data: Option[String])

  /**
    * @see [[https://core.telegram.org/bots/api#answercallbackquery]]
    */
  final case class TCallbackAnswer(callbackQueryId: String)

  /**
    * @see [[https://core.telegram.org/bots/api#available-methods]]
    */
  final case class TResponse[A](ok: Boolean, result: A)

  /**
    * @see [[https://core.telegram.org/bots/api#editmessagetext]]
    */
  final case class TEditMessage(
      chatId: Long,
      messageId: Long,
      text: String,
      replyMarkup: Option[TInlineKeyboardMarkup] = None,
      parseMode: String = "Markdown"
  )
}
