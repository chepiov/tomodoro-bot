package org.chepiov.tomodoro.actors

import java.io.IOException
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.time.OffsetDateTime

import akka.actor.{ActorIdentity, ActorSystem, Identify}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.chepiov.tomodoro.actors.UserActor.{CommandMsg, ChatMsg}
import org.chepiov.tomodoro.algebras.User._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class UserActorSpec
    extends TestKit(ActorSystem("test-system", ConfigFactory.load("application-persistence-test"))) with WordSpecLike
    with Matchers with BeforeAndAfterAll with ImplicitSender {

  override def beforeAll(): Unit = {
    List(
      "akka.persistence.journal.leveldb.dir",
      "akka.persistence.snapshot-store.local.dir"
    ).filter { s =>
      val path = Paths.get(system.settings.config.getString(s))
      Files.exists(path) && Files.isDirectory(path)
    }.foreach { s =>
      Files.walkFileTree(
        Paths.get(system.settings.config.getString(s)),
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }
          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
    }
  }

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  def currentTime: Long = OffsetDateTime.now().toEpochSecond

  val defaultSettings: UserSettings = UserSettings(4, 1, 2, 4)

  "UserActor" should {
    "" in {
      val messengerActor = TestProbe("messenger")
      val userActor =
        system.actorOf(UserActor.props(1, system.actorSelection(messengerActor.ref.path), SECONDS, defaultSettings))

      // awaiting full creation for timing
      system.actorSelection(userActor.path) ! Identify(1)
      expectMsg(ActorIdentity(1, Some(userActor)))

      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      @volatile var c = false
      userActor ! CommandMsg(Continue(currentTime), () => c = true)
      awaitCond(c)
      messengerActor.expectMsgType[ChatMsg]
    }
  }
}
