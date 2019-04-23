package org.chepiov.tomodoro.interpreters

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, RequestEntity}
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.{ActorMaterializer, Materializer}
import akka.testkit.TestKit
import cats.effect.IO
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram._
import org.scalatest.{Assertion, AsyncWordSpecLike, BeforeAndAfterAll, Matchers}
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class TelegramJsonSpec
    extends TestKit(ActorSystem("TelegramSpec")) with AsyncWordSpecLike with Matchers with BeforeAndAfterAll {
  import TelegramJsonSpec._
  import TelegramJsonSupport._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  implicit val ec: ExecutionContext = system.dispatcher
  implicit val mat: Materializer    = ActorMaterializer()

  implicit def toFuture(io: IO[Assertion]): Future[Assertion] =
    io.unsafeToFuture()

  "Telegram ReplyKeyboardMessage" should {
    "be serialized correctly" in {
      replyKeyboardMessage.toJson shouldBe replyKeyboardMessageJson
    }

    "be de-serialized correctly" in {
      replyKeyboardMessageJson.convertTo[TSendMessage] shouldBe replyKeyboardMessage
    }

    "be serialized to entity correctly" in {
      for {
        entity <- IO.fromFuture(IO(Marshal(replyKeyboardMessage).to[RequestEntity]))
        strict <- IO.fromFuture(IO(entity.toStrict(5.seconds)))
        json   = strict.data.utf8String.parseJson
      } yield json shouldBe replyKeyboardMessageJson
    }
  }

  "Telegram TResponse" should {
    "be de-serialized from entity correctly" in {
      for {
        entity   <- IO(HttpEntity(ContentTypes.`application/json`, userMessageJson.compactPrint))
        response <- IO.fromFuture(IO(Unmarshal(entity).to[TResponse[TUser]]))
      } yield response shouldBe tResponseMessage
    }
  }
}

case object TelegramJsonSpec {
  val replyKeyboardMessage: TSendMessage = TSendMessage(
    1,
    "test",
    TReplyKeyboardMarkup(
      List(
        List(TKeyboardButton("/settings"), TKeyboardButton("/statistics"), TKeyboardButton("/help")),
        List(TKeyboardButton("/reset"), TKeyboardButton("/about"))
      )
    ).some
  )

  val tResponseMessage: TResponse[TUser] = TResponse(
    ok = true,
    result = TUser(659848541L, isBot = true, "Tomodoro", None, Some("TomodoroBot"))
  )

  val replyKeyboardMessageJson: JsValue =
    """
      |{
      |  "chat_id": 1,
      |  "reply_markup": {
      |    "keyboard": [
      |      [
      |        {
      |          "text": "/settings"
      |        },
      |        {
      |          "text": "/statistics"
      |        },
      |        {
      |          "text": "/help"
      |        }
      |      ],
      |      [
      |        {
      |          "text": "/reset"
      |        },
      |        {
      |          "text": "/about"
      |        }
      |      ]
      |    ]
      |  },
      |  "parse_mode":"Markdown",
      |  "text": "test"
      |}
    """.stripMargin.parseJson

  val userMessageJson: JsValue =
    """
    |{
    |    "ok": true,
    |    "result": {
    |        "first_name": "Tomodoro",
    |        "id": 659848541,
    |        "is_bot": true,
    |        "username": "TomodoroBot"
    |    }
    |}
  """.stripMargin.parseJson
}
