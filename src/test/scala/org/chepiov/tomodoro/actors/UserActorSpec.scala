package org.chepiov.tomodoro.actors

import java.time.OffsetDateTime

import akka.actor.{ActorIdentity, ActorRef, ActorSelection, ActorSystem, Identify, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.chepiov.tomodoro.actors.UserActor.{ChatMsg, ChatMsgConfirm, CommandMsg, QueryMsg}
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras.Users.defaultUserSettings
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class UserActorSpec
    extends TestKit(
      ActorSystem(
        "user-test-system",
        ConfigFactory
          .parseString(PersistenceSpec.config("user"))
          .withFallback(ConfigFactory.load())
      )
    ) with WordSpecLike with Matchers with BeforeAndAfterAll with ImplicitSender with PersistenceSpec {
  import UserActorSpec._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  def currentTime: Long = OffsetDateTime.now().toEpochSecond

  val ack: () => Unit = () => ()

  val defaultSettings: UserSettings = UserSettings(3, 1, 2, 4)

  "UserActor" should {
    "change state accordingly commands" in {
      val messengerActor = TestProbe("messenger")
      val stateActor     = TestProbe("state")
      val userActor =
        system.actorOf(
          props(stateActor.ref, 1L, system.actorSelection(messengerActor.ref.path), SECONDS, defaultSettings)
        )

      // awaiting full creation for timing
      system.actorSelection(userActor.path) ! Identify(1)
      expectMsg(ActorIdentity(1, Some(userActor)))

      // start
      userActor ! CommandMsg(Continue(currentTime), ack)
      val sentMsg = messengerActor.expectMsgType[ChatMsg]
      messengerActor.reply(ChatMsgConfirm(sentMsg.deliveryId))
      val working1 = stateActor.expectMsgType[UserState]
      working1.status shouldBe a[Working]
      working1.status.remaining shouldBe 3

      expectNoMessage(3.second)

      // waiting short break
      messengerActor.expectMsgType[ChatMsg].msg.chatId shouldBe 1L
      val waitingShortBreak = stateActor.expectMsgType[UserState]
      waitingShortBreak.status shouldBe a[WaitingBreak]
      waitingShortBreak.status.remaining shouldBe 3

      // short break
      userActor ! CommandMsg(Continue(currentTime), ack)
      messengerActor.expectMsgType[ChatMsg]
      val shortBreak = stateActor.expectMsgType[UserState]
      shortBreak.status shouldBe a[Breaking]
      shortBreak.status.remaining shouldBe 3

      expectNoMessage(1.second)

      // waiting work
      messengerActor.expectMsgType[ChatMsg].msg.chatId shouldBe 1L
      val waitingWork = stateActor.expectMsgType[UserState]
      waitingWork.status shouldBe a[WaitingWork]
      waitingWork.status.remaining shouldBe 3

      // working
      userActor ! CommandMsg(Continue(currentTime), ack)
      messengerActor.expectMsgType[ChatMsg]
      val working2 = stateActor.expectMsgType[UserState]
      working2.status shouldBe a[Working]
      working2.status.remaining shouldBe 2

      userActor ! CommandMsg(Skip(currentTime), ack)
      messengerActor.expectMsgType[ChatMsg]
      val waitingBreak2 = stateActor.expectMsgType[UserState]
      waitingBreak2.status shouldBe a[WaitingBreak]
      waitingBreak2.status.remaining shouldBe 2

      // settings
      userActor ! CommandMsg(SetSettings(currentTime), ack)
      messengerActor.expectMsgType[ChatMsg]
      stateActor.expectNoMessage()
    }
  }

  "not change state accordingly queries" in {
    val messengerActor = TestProbe("messenger")
    val stateActor     = TestProbe("state")
    val userActor =
      system.actorOf(
        props(stateActor.ref, 2L, system.actorSelection(messengerActor.ref.path), SECONDS, defaultSettings)
      )

    userActor ! QueryMsg(GetHelp, ack)
    messengerActor.expectMsgType[ChatMsg]
    stateActor.expectNoMessage()

    userActor ! QueryMsg(GetStats, ack)
    messengerActor.expectMsgType[ChatMsg]
    stateActor.expectNoMessage()

    userActor ! QueryMsg(GetState, ack)
    messengerActor.expectMsgType[ChatMsg]
    stateActor.expectNoMessage()
  }
}

case object UserActorSpec {
  class TestUserActor(
      stateProbe: ActorRef,
      chatId: Long,
      messenger: ActorSelection,
      timeUnit: TimeUnit,
      defaultSettings: UserSettings,
      snapShotInterval: Int
  ) extends UserActor(chatId, messenger, timeUnit, defaultSettings, snapShotInterval) {
    override def updateState(newState: UserState): Unit = {
      stateProbe ! newState
      super.updateState(newState)
    }
  }

  def props(
      stateProbe: ActorRef,
      chatId: Long,
      chat: ActorSelection,
      timeUnit: TimeUnit = MINUTES,
      defaultSettings: UserSettings = defaultUserSettings,
      snapshotInterval: Int = 1000
  ): Props =
    Props(new TestUserActor(stateProbe, chatId, chat, timeUnit, defaultSettings, snapshotInterval))
}
