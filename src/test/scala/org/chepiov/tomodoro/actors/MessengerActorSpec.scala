package org.chepiov.tomodoro.actors

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import cats.effect.IO
import com.typesafe.config.ConfigFactory
import org.chepiov.tomodoro.actors.UserActor.{ChatMsg, ChatMsgConfirm}
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.util.{Failure, Success}

class MessengerActorSpec
    extends TestKit(ActorSystem("test-system", ConfigFactory.load("application-persistence-test"))) with WordSpecLike
    with Matchers with BeforeAndAfterAll with ImplicitSender {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "MessengerActor" should {
    "deliver message to user" in {

      val actor = system.actorOf(MessengerActor.props[IO](_ => IO(Success(()))))

      actor ! ChatMsg(1L, TSendMessage(1L, ""))

      expectMsg(ChatMsgConfirm(1L))
    }

    "handle error" in {

      val actor = system.actorOf(MessengerActor.props[IO](_ => IO(Failure[Unit](new RuntimeException()))))

      actor ! ChatMsg(1L, TSendMessage(1L, ""))

      expectMsgType[akka.actor.Status.Failure]
    }
  }
}
