package org.chepiov.tomodoro.interpreters

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import cats.Id
import cats.effect.{ContextShift, IO}
import cats.syntax.flatMap._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.actors.UserActor.{CommandMsg, QueryMsg}
import org.chepiov.tomodoro.algebras.User.{Continue, GetState}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

//noinspection AppropriateActorConstructorNotFound
@SuppressWarnings(
  Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Product",
    "org.wartremover.warts.Serializable"
  )
)
class UserInterpreterSpec
    extends TestKit(ActorSystem("test-system")) with WordSpecLike with Matchers with BeforeAndAfterAll
    with ImplicitSender {
  import UserInterpreterSpec._

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val userActor: ActorRef    = system.actorOf(Props(classOf[SuccessUserActor], testActor))
  val badUserActor: ActorRef = system.actorOf(Props[FailureUserActor])

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  implicit def logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  "User" should {
    "deliver actions to actor" in {
      val program: IO[Unit] = for {
        user <- UserInterpreter[Id, IO](1L, userActor)
        _    <- user.advance(Continue(0))
        _    <- user.info(GetState)
      } yield {
        expectMsgAllOf(Continue(0), GetState)
        ()
      }
      program.unsafeRunSync()
    }
  }

  it should {
    "handle errors" in {
      val program: IO[Unit] = for {
        user    <- UserInterpreter[Id, IO](1L, badUserActor)
        advance <- IO.race(user.advance(Continue(0)), IO.timer(ExecutionContext.global).sleep(3.seconds))
        query   <- IO.race(user.info(GetState), IO.timer(ExecutionContext.global).sleep(3.seconds))
      } yield {
        advance.isRight shouldBe true
        query.isRight shouldBe true
        ()
      }
      program.unsafeRunSync()
    }
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Any", "org.wartremover.warts.Throw"))
case object UserInterpreterSpec {
  class SuccessUserActor(probe: ActorRef) extends Actor {
    override def receive: Receive = {
      case CommandMsg(cmd, ack) =>
        probe ! cmd
        ack()
      case QueryMsg(query, ack) =>
        probe ! query
        ack()
    }
  }

  class FailureUserActor extends Actor {
    override def receive: Receive = {
      case CommandMsg(_, _) => throw new RuntimeException()
      case QueryMsg(_, _)   => throw new RuntimeException()
    }
  }
}
