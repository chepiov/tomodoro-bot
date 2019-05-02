package org.chepiov.tomodoro.actors

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorIdentity, ActorRef, ActorSystem, Identify, Props}
import akka.testkit.{ImplicitSender, TestKit}
import cats.effect.IO
import com.typesafe.config.ConfigFactory
import org.chepiov.tomodoro.actors.UsersActor.GetUser
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.programs.UserActivity.StateChangedEvent
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.util.{Success, Try}

//noinspection AppropriateActorConstructorNotFound
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class UsersActorSpec
    extends TestKit(
      ActorSystem(
        "users-test-system",
        ConfigFactory
          .parseString(PersistenceSpec.config("users"))
          .withFallback(ConfigFactory.load())
      )
    ) with WordSpecLike with Matchers with BeforeAndAfterAll with ImplicitSender with PersistenceSpec {
  import UsersActorSpec._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val messenger: TSendMessage => IO[Unit]          = _ => IO.unit
  val activity: StateChangedEvent => IO[Try[Unit]] = _ => IO(Success(()))

  "UsersActor" should {
    "create new user or return existing" in {

      val usersActor = system.actorOf(Props(classOf[TestUsersActor], testActor, messenger, activity))

      // awaiting full creation for timing
      system.actorSelection(usersActor.path) ! Identify(1)
      expectMsg(ActorIdentity(1, Some(usersActor)))

      usersActor ! GetUser(1L, _ => ())
      val (chatId, userActor) = expectMsgType[(Long, ActorRef)]
      chatId shouldBe 1L

      val hook = new AtomicReference[Option[ActorRef]](None)
      usersActor ! GetUser(1L, ref => hook.set(Some(ref)))

      expectNoMessage()
      awaitAssert(hook.get().isDefined)

      hook.get() shouldBe Some(userActor)
    }
  }
}

case object UsersActorSpec {
  class TestUsersActor(
      probe: ActorRef,
      messenger: TSendMessage => IO[Unit],
      activity: StateChangedEvent => IO[Try[Unit]]
  ) extends UsersActor[IO](messenger, activity) {
    override def updateState(
        chatId: Long,
        userActor: ActorRef,
        chatIdToUser: Map[Long, ActorRef],
        userToChatId: Map[ActorRef, Long]
    ): Unit = {
      probe.tell((chatId, userActor), self)
      super.updateState(chatId, userActor, chatIdToUser, userToChatId)
    }
  }
}
