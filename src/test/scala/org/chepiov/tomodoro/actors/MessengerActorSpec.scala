package org.chepiov.tomodoro.actors

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import cats.effect.IO
import com.typesafe.config.ConfigFactory
import org.chepiov.tomodoro.actors.UserActor.{ChatMsg, ChatMsgConfirm, ChatMsgError}
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class MessengerActorSpec
    extends TestKit(
      ActorSystem(
        "messenger-test-system",
        ConfigFactory
          .parseString(PersistenceSpec.config("messenger"))
          .withFallback(ConfigFactory.load())
      )
    ) with WordSpecLike with Matchers with BeforeAndAfterAll with ImplicitSender with PersistenceSpec {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "MessengerActor" should {
    "deliver message to user" in {

      val actor = system.actorOf(MessengerActor.props[IO](_ => IO.unit))

      actor ! ChatMsg(1L, TSendMessage(1L, ""))

      expectMsg(ChatMsgConfirm(1L))
    }

    "handle error" in {

      val actor = system.actorOf(MessengerActor.props[IO](_ => IO.raiseError(new RuntimeException())))

      actor ! ChatMsg(1L, TSendMessage(1L, ""))

      expectMsgType[ChatMsgError]
    }
  }
}
