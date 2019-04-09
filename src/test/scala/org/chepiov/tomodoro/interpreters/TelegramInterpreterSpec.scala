package org.chepiov.tomodoro.interpreters

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.{ImplicitSender, TestKit}
import cats.effect._
import cats.effect.concurrent._
import cats.syntax.option._
import org.chepiov.tomodoro.TomodoroSpec
import org.chepiov.tomodoro.algebra.Telegram.Settings
import org.chepiov.tomodoro.algebra.{Logger, Telegram}
import org.chepiov.tomodoro.interpreters.TestTelegramInterpreter.RequestDesc
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class TelegramInterpreterSpec
    extends TestKit(ActorSystem("TelegramSpec")) with ImplicitSender with WordSpecLike with Matchers
    with BeforeAndAfterAll with TomodoroSpec {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A telegram" must {
    "send help" in {
      val (chatId, text, messageId) = runTelegram(telegram => telegram.help(1, 2L.some))
      chatId should be(1)
      text should be(Telegram.helpMessage)
      messageId shouldEqual Some(2L)
    }

    "send run" in {
      val (chatId, text, _) = runTelegram(telegram => telegram.run(1))
      chatId should be(1)
      text should be(Telegram.ranMessage)
    }

    "send end" in {
      val (chatId, text, _) = runTelegram(telegram => telegram.end(1))
      chatId should be(1)
      text should be(Telegram.endedMessage)
    }

    "send custom" in {
      val (chatId, text, messageId) = runTelegram(telegram => telegram.custom(1, "victoria concordia crescit", 2L.some))
      chatId should be(1)
      text should be("victoria concordia crescit")
      messageId shouldEqual Some(2L)
    }

    "send settings" in {
      val (chatId, text, _) = runTelegram(telegram => telegram.settings(1, Settings(1, 2, 3)))
      chatId should be(1)
      text should be(Telegram.settingsMessage(Settings(1, 2, 3)))
    }
  }

  private def runTelegram(logic: TestTelegramInterpreter => IO[Unit]): RequestDesc = {
    val r = for {
      telegram <- TestTelegramInterpreter()
      _        <- logic(telegram)
      result   <- telegram.result.take
    } yield result
    r.unsafeRunSync()
  }
}

class TestTelegramInterpreter(config: TelegramConfig, logger: Logger[IO], val result: MVar[IO, RequestDesc])(
    implicit actorSystem: ActorSystem
) extends TelegramInterpreter[IO](config, logger) {

  override def sendMessage(chatId: Long, text: String, messageIdReplyTo: Option[Long]): IO[Unit] =
    for {
      _ <- IO.unit
      _ <- result.put((chatId, text, messageIdReplyTo))
    } yield ()

}

case object TestTelegramInterpreter {
  type RequestDesc = (Long, String, Option[Long])
  val config = TelegramConfig("test")
  val logger = new LoggerInterpreter[IO]("test")

  def apply()(
      implicit actorSystem: ActorSystem
  ): IO[TestTelegramInterpreter] = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ActorMaterializer().executionContext)
    for {
      req      <- MVar[IO].empty[RequestDesc]
      telegram = new TestTelegramInterpreter(config, logger, req)
    } yield telegram
  }
}
