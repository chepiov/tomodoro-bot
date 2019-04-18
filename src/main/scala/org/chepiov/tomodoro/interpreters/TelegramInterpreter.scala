package org.chepiov.tomodoro.interpreters

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.{Marshal, Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import cats.Monad
import cats.effect.{Async, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram
import org.chepiov.tomodoro.algebras.Telegram._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class TelegramInterpreter[F[_]: Logger: Async](config: TelegramConfig)(
    implicit actorSystem: ActorSystem
) extends Telegram[F] {
  import TelegramJsonSupport._

  private val uri = s"${config.scheme}://${config.host}/bot${config.token}"

  private val matF = Async[F].delay(ActorMaterializer())
  private val ecF  = Async[F].delay(actorSystem.dispatcher)

  override def sendMessage(message: TSendMessage): F[Unit] =
    for {
      request  <- marshal(message, "sendMessage")
      _        <- Logger[F].debug(s"[${message.chatId}] sendMessage method call")
      response <- getResponse(request)
      _        <- checkResponse(response, "sendMessage")
      _        <- discard(response)
      _        <- Logger[F].debug(s"[${message.chatId}] sendMessage result status: ${response.status}")

    } yield ()

  override def answerCallbackQuery(answer: TCallbackAnswer): F[Unit] = Sync[F].unit

  override def getMe: F[TUser] =
    for {
      request  <- Async[F].delay(HttpRequest(method = HttpMethods.GET, uri = Uri(s"$uri/getMe")))
      _        <- Logger[F].debug(s"getMe method call")
      response <- getResponse(request)
      _        <- checkResponse(response, "getMe")
      _        <- Logger[F].debug(s"getMe result status: ${response.status}")
      resp     <- unmarshal[TResponse[TUser]](response)
    } yield resp.result

  private def getResponse(request: HttpRequest): F[HttpResponse] =
    for {
      implicit0(ec: ExecutionContext) <- ecF
      response <- Async[F].async[HttpResponse] { k =>
                   Http().singleRequest(request).onComplete {
                     case Success(resp) => k(Right(resp))
                     case Failure(e)    => k(Left(e))
                   }
                 }
    } yield response

  private def checkResponse(response: HttpResponse, method: String): F[Unit] =
    for {
      result <- if (response.status != StatusCodes.OK)
                 for {
                   _ <- Logger[F].error(s"Error during $method, status code: ${response.status}")
                   _ <- discard(response)
                   r <- error(response, method)
                 } yield r
               else Async[F].unit
    } yield result

  private def discard(response: HttpResponse): F[Unit] =
    for {
      implicit0(mat: Materializer) <- matF
      _                            <- Async[F].delay(response.discardEntityBytes())
    } yield ()

  private def error(response: HttpResponse, method: String): F[Unit] =
    Async[F].raiseError[Unit](new RuntimeException(s"Error during $method, status code: ${response.status}"))

  private def marshal(message: TSendMessage, path: String): F[HttpRequest] =
    for {
      implicit0(ec: ExecutionContext) <- ecF
      entity <- Async[F].async[RequestEntity] { k =>
                 Marshal(message).to[RequestEntity].onComplete {
                   case Success(r) => k(Right(r))
                   case Failure(e) => k(Left(e))
                 }
               }
    } yield HttpRequest(method = HttpMethods.POST, uri = Uri(s"$uri/$path"), entity = entity)

  private def unmarshal[A: RootJsonFormat](response: HttpResponse): F[A] =
    for {
      implicit0(ec: ExecutionContext) <- ecF
      implicit0(mat: Materializer)    <- matF
      result <- Async[F].async[A] { k =>
                 Unmarshal(response.entity).to[A].onComplete {
                   case Success(r) => k(Right(r))
                   case Failure(e) => k(Left(e))
                 }
               }
    } yield result
}

private[interpreters] case object TelegramJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  import spray.json._

  implicit val replyKeyboardButton: RootJsonFormat[TKeyboardButton]        = jsonFormat1(TKeyboardButton)
  implicit val replyKeyboardMarkup: RootJsonFormat[TReplyKeyboardMarkup]   = jsonFormat1(TReplyKeyboardMarkup)
  implicit val inlineKeyboardButton: RootJsonFormat[TInlineKeyboardButton] = jsonFormat1(TInlineKeyboardButton)
  implicit val inlineKeyboardMarkup: RootJsonFormat[TInlineKeyboardMarkup] = jsonFormat1(TInlineKeyboardMarkup)

  implicit val userFormat: RootJsonFormat[TUser] =
    jsonFormat(TUser.apply, "id", "is_bot", "first_name", "last_name", "username")

  implicit def responseFormat[A: RootJsonFormat]: RootJsonFormat[TResponse[A]] =
    jsonFormat2(TResponse.apply[A])

  implicit def keyboardMarkup: RootJsonFormat[TReplyMarkup] = new RootJsonFormat[TReplyMarkup] {
    override def write(obj: TReplyMarkup): JsValue =
      obj match {
        case TReplyKeyboardMarkup(k) =>
          JsObject("keyboard" -> k.toJson)
        case TInlineKeyboardMarkup(k) =>
          JsObject("inline_keyboard" -> k.toJson)
      }

    override def read(json: JsValue): TReplyMarkup = {
      val fields = json.asJsObject().fields
      fields match {
        case _ if fields.contains("keyboard") =>
          TReplyKeyboardMarkup(fields("keyboard").convertTo[List[List[TKeyboardButton]]])
        case _ if fields.contains("inline_keyboard") =>
          TInlineKeyboardMarkup(fields("keyboard").convertTo[List[List[TInlineKeyboardButton]]])
        case _ =>
          throw DeserializationException("Unknown keyboard markup")
      }
    }
  }

  implicit def sendMessage: RootJsonFormat[TSendMessage] =
    jsonFormat(TSendMessage, "chat_id", "text", "reply_markup")

  implicit def sendMessageToEntity: ToEntityMarshaller[TSendMessage] =
    Marshaller.withFixedContentType(MediaTypes.`application/json`) { a =>
      HttpEntity(ContentTypes.`application/json`, a.toJson.compactPrint)
    }
}

case object TelegramInterpreter {
  def apply[I[_]: Monad, F[_]: Logger: Async](
      config: TelegramConfig
  )(implicit actorSystem: ActorSystem): I[Telegram[F]] =
    for {
      _ <- Monad[I].unit
      t = new TelegramInterpreter[F](config)
    } yield t

  def apply[F[_]: Logger: Async](
      config: TelegramConfig
  )(implicit actorSystem: ActorSystem): F[Telegram[F]] = apply[F, F](config)
}
