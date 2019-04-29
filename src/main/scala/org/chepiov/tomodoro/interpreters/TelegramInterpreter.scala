package org.chepiov.tomodoro.interpreters

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.{Marshal, Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import cats.Monad
import cats.effect.Async
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Telegram
import org.chepiov.tomodoro.algebras.Telegram._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}

class TelegramInterpreter[F[_]: Logger: Async](config: TelegramConfig, actorSystem: ActorSystem) extends Telegram[F] {
  import TelegramJsonSupport._

  private val uri = s"${config.scheme}://${config.host}/bot${config.token}"

  implicit private val system: ActorSystem  = actorSystem
  implicit private val mat: Materializer    = ActorMaterializer()
  implicit private val ec: ExecutionContext = actorSystem.dispatcher

  override def sendMessage(message: TSendMessage): F[Unit] =
    for {
      request  <- marshal(message, "sendMessage")
      _        <- Logger[F].debug(s"[${message.chatId}] sendMessage method call")
      _        <- Logger[F].trace(s"[${message.chatId}] Request: $request")
      response <- getResponse(request)
      _        <- Logger[F].trace(s"[${message.chatId}] Response: $response")
      _        <- checkResponse(response, "sendMessage")
      _        <- discard(response)
      r        <- Logger[F].debug(s"[${message.chatId}] sendMessage result status: ${response.status}")
    } yield r

  override def answerCallbackQuery(answer: TCallbackAnswer): F[Unit] =
    for {
      request  <- marshal(answer, "answerCallbackQuery")
      _        <- Logger[F].debug(s"answerCallbackQuery method call")
      _        <- Logger[F].trace(s"Request: $request")
      response <- getResponse(request)
      _        <- Logger[F].trace(s"Response: $response")
      _        <- checkResponse(response, "answerCallbackQuery")
      r        <- Logger[F].debug(s"answerCallbackQuery result status: ${response.status}")
    } yield r

  override def editMessageText(message: TEditMessage): F[Unit] =
    for {
      request  <- marshal(message, "editMessageText")
      _        <- Logger[F].debug(s"[${message.chatId}] editMessageText method call")
      _        <- Logger[F].trace(s"[${message.chatId}] Request: $request")
      response <- getResponse(request)
      _        <- Logger[F].trace(s"[${message.chatId}] Response: $response")
      _        <- checkResponse(response, "editMessageText")
      r        <- Logger[F].debug(s"editMessageText result status: ${response.status}")
    } yield r

  override def getMe: F[TUser] =
    for {
      request  <- Async[F].delay(HttpRequest(method = HttpMethods.GET, uri = Uri(s"$uri/getMe")))
      _        <- Logger[F].debug(s"getMe method call")
      _        <- Logger[F].trace(s"Request: $request")
      response <- getResponse(request)
      _        <- Logger[F].trace(s"Response: $response")
      _        <- checkResponse(response, "getMe")
      _        <- Logger[F].debug(s"getMe result status: ${response.status}")
      resp     <- unmarshal[TResponse[TUser]](response)
    } yield resp.result

  private def getResponse(request: HttpRequest): F[HttpResponse] =
    Async[F].async[HttpResponse] { k =>
      Http().singleRequest(request).onComplete {
        case Success(resp) => k(Right(resp))
        case Failure(e)    => k(Left(e))
      }
    }

  private def checkResponse(response: HttpResponse, method: String): F[Unit] =
    if (response.status != StatusCodes.OK)
      for {
        _ <- Logger[F].error(s"Error during $method, status code: ${response.status}")
        e <- unmarshalToString(response)
        _ <- Logger[F].error(s"Error: $e")
        r <- error(response, method)
      } yield r
    else Async[F].unit

  private def discard(response: HttpResponse): F[Unit] =
    Async[F].delay(response.discardEntityBytes()).map(_ => ())

  private def error(response: HttpResponse, method: String): F[Unit] =
    Async[F].raiseError[Unit](new RuntimeException(s"Error during $method, status code: ${response.status}"))

  private def marshal[A: RootJsonFormat](message: A, path: String): F[HttpRequest] =
    for {
      entity <- Async[F].async[RequestEntity] { k =>
                 Marshal(message).to[RequestEntity].onComplete {
                   case Success(r) => k(Right(r))
                   case Failure(e) => k(Left(e))
                 }
               }
    } yield HttpRequest(method = HttpMethods.POST, uri = Uri(s"$uri/$path"), entity = entity)

  private def unmarshal[A: RootJsonFormat](response: HttpResponse): F[A] =
    Async[F].async[A] { k =>
      Unmarshal(response.entity).to[A].onComplete {
        case Success(r) => k(Right(r))
        case Failure(e) => k(Left(e))
      }
    }

  private def unmarshalToString(response: HttpResponse): F[String] =
    Async[F].async[String] { k =>
      Unmarshal(response.entity).to[String].onComplete {
        case Success(r) => k(Right(r))
        case Failure(e) => k(Left(e))
      }
    }
}

private[interpreters] case object TelegramJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  import spray.json._

  implicit val replyKeyboardButton: RootJsonFormat[TKeyboardButton] = jsonFormat1(TKeyboardButton)

  implicit val replyKeyboardMarkup: RootJsonFormat[TReplyKeyboardMarkup] = jsonFormat1(TReplyKeyboardMarkup)

  implicit val inlineKeyboardButton: RootJsonFormat[TInlineKeyboardButton] =
    jsonFormat(TInlineKeyboardButton.apply, "text", "callback_data")

  implicit val inlineKeyboardMarkup: RootJsonFormat[TInlineKeyboardMarkup] =
    jsonFormat(TInlineKeyboardMarkup.apply, "inline_keyboard")

  implicit val userFormat: RootJsonFormat[TUser] =
    jsonFormat(TUser.apply, "id", "is_bot", "first_name", "last_name", "username")

  implicit val callbackAnswer: RootJsonFormat[TCallbackAnswer] =
    jsonFormat(TCallbackAnswer.apply, "callback_query_id")

  implicit def responseFormat[A: RootJsonFormat]: RootJsonFormat[TResponse[A]] =
    jsonFormat2(TResponse.apply[A])

  implicit def keyboardMarkup: RootJsonFormat[TReplyMarkup] = new RootJsonFormat[TReplyMarkup] {
    override def write(obj: TReplyMarkup): JsValue =
      obj match {
        case kb: TReplyKeyboardMarkup =>
          kb.toJson
        case kb: TInlineKeyboardMarkup =>
          kb.toJson
      }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
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

  implicit val sendMessageFormat: RootJsonFormat[TSendMessage] =
    jsonFormat(TSendMessage, "chat_id", "text", "reply_markup", "parse_mode")

  implicit val editMessageFormat: RootJsonFormat[TEditMessage] =
    jsonFormat(TEditMessage, "chat_id", "message_id", "text", "reply_markup", "parse_mode")

  implicit val sendMessageToEntity: ToEntityMarshaller[TSendMessage] =
    Marshaller.withFixedContentType(MediaTypes.`application/json`) { a =>
      HttpEntity(ContentTypes.`application/json`, a.toJson.compactPrint)
    }
}

case object TelegramInterpreter {

  def apply[I[_]: Monad, F[_]: Logger: Async](
      config: TelegramConfig,
      actorSystem: ActorSystem
  ): I[Telegram[F]] =
    (new TelegramInterpreter[F](config, actorSystem): Telegram[F]).pure[I]

  def apply[F[_]: Async](
      config: TelegramConfig,
      actorSystem: ActorSystem
  ): F[Telegram[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      t                            <- apply[F, F](config, actorSystem)
    } yield t
}
