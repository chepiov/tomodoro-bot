package org.chepiov.tomodoro

//import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.syntax.option._
import org.scalatest.{Matchers, WordSpec}

class MainSpec extends WordSpec with Matchers with ScalatestRouteTest {
/*  "The updates API" should {
    "accept help command" in {
      val t = for {
        updates    <- Updates(Nil)
        appRoutes  = Main.route(updates)
        httpEntity = HttpEntity(ContentTypes.`application/json`, messages.helpRequest)
        result     = Post("/updates", httpEntity) ~> appRoutes
        allUpdates <- updates.allUpdates
      } yield {
        result ~> check {
          responseAs[String] should be("")
          allUpdates.contains(messages.helpRequestObject) should be(true)
        }
      }
      t.unsafeRunSync()
    }
  }*/
}

object messages {
  val helpRequest: String =
    """
      |{
      |  "update_id": 572825922,
      |  "message": {
      |    "message_id": 5,
      |    "from": {
      |      "id": 80291896,
      |      "is_bot": false,
      |      "first_name": "Anvar",
      |      "last_name": "Kiekbaev",
      |      "username": "chepiov",
      |      "language_code": "en"
      |    },
      |    "chat": {
      |      "id": 80291896,
      |      "first_name": "Anvar",
      |      "last_name": "Kiekbaev",
      |      "username": "chepiov",
      |      "type": "private"
      |    },
      |    "date": 1554288823,
      |    "text": "/help"
      |  }
      |}
    """.stripMargin
  val helpRequestObject = BotUpdate(572825922L, BotMessage(5L, BotChat(80291896L), "/help".some).some)
}
