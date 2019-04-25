package org.chepiov.tomodoro.programs

import org.chepiov.tomodoro.algebras.Repository.ActivityDescriptor._
import org.chepiov.tomodoro.algebras.Repository.ActivityLog
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.programs.UserActivity.{at, StateChangedEvent}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, OptionValues, PropSpec}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class UserActivitySpec extends PropSpec with Matchers with PropertyChecks with OptionValues {
  import UserActivitySpec._

  property("Event about starting working must create correct activity log") {
    forAll(stateEventGen(Continue, Working)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      log.value.descriptor.entryName shouldBe TomodoroStarted.entryName
    }
  }

  property("Event about finishing working must create correct activity log") {
    forAll(stateEventGen(Finish, WaitingBreak)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      if (event.state.status.remaining == event.state.settings.amount)
        log.value.descriptor.entryName shouldBe CycleFinished.entryName
      else
        log.value.descriptor.entryName shouldBe TomodoroFinished.entryName
    }
  }

  property("Event about starting break must create correct activity log") {
    forAll(stateEventGen(Continue, Breaking)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      if (event.state.status.remaining == 0)
        log.value.descriptor.entryName shouldBe LongBreakStarted.entryName
      else
        log.value.descriptor.entryName shouldBe ShortBreakStarted.entryName
    }
  }

  property("Event about finishing break must create correct activity log") {
    forAll(stateEventGen(Finish, WaitingWork)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      if (event.state.status.remaining == event.state.settings.amount)
        log.value.descriptor.entryName shouldBe LongBreakFinished.entryName
      else
        log.value.descriptor.entryName shouldBe ShortBreakFinished.entryName
    }
  }

  property("Event about pausing working must create correct activity log") {
    forAll(suspendedStateEventGen(Suspend, WorkSuspended)) { event =>
      val log = UserActivity.createLog(event)
      checkWithSuspendTime(event, log)
      log.value.descriptor.entryName shouldBe TomodoroPaused.entryName
    }
  }

  property("Event about pausing break must create correct activity log") {
    forAll(suspendedStateEventGen(Suspend, BreakSuspended)) { event =>
      val log = UserActivity.createLog(event)
      checkWithSuspendTime(event, log)
      log.value.descriptor.entryName shouldBe BreakPaused.entryName
    }
  }

  property("Event about skipping working must create correct activity log") {
    forAll(stateEventGen(Skip, WaitingBreak)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      log.value.descriptor.entryName shouldBe TomodoroSkipped.entryName
    }
  }

  property("Event about skipping break must create correct activity log") {
    forAll(stateEventGen(Skip, WaitingWork)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      log.value.descriptor.entryName shouldBe BreakSkipped.entryName
    }
  }

  property("Event about reset must create correct activity log") {
    forAll(stateEventAnyStatusGen(Reset)) { event =>
      val log = UserActivity.createLog(event)
      checkWithStartTime(event, log)
      log.value.descriptor.entryName shouldBe CycleReset.entryName
    }
  }

  property("Event about updating settings must create correct activity log") {
    forAll(stateEventAnyStatusGen(SetSettingsValue)) { event =>
      val log = UserActivity.createLog(event)
      log shouldBe 'defined
      log.value.chatId shouldBe event.chatId
      log.value.time shouldBe at(event.cmd.time)
      log.value.descriptor.entryName shouldBe SettingsUpdated.entryName
    }
  }

  private def checkWithSuspendTime(event: StateChangedEvent, log: Option[ActivityLog]): Assertion = {
    log shouldBe 'defined
    log.value.chatId shouldBe event.chatId
    log.value.time shouldBe at(event.state.status.asInstanceOf[SuspendedStatus].suspend)
  }

  private def checkWithStartTime(event: StateChangedEvent, log: Option[ActivityLog]): Assertion = {
    log shouldBe 'defined
    log.value.chatId shouldBe event.chatId
    log.value.time shouldBe at(event.state.status.startTime)
  }
}

case object UserActivitySpec {
  import UserStateMachineSpec._

  def stateEventGen[C <: Command, S <: Status](cb: Long => C, sb: (Int, Long, Long) => S): Gen[StateChangedEvent] =
    for {
      chatId       <- Gen.posNum[Long]
      (cmd, state) <- commandAndStateGen(cb, sb)
    } yield StateChangedEvent(chatId, state, cmd)

  def stateEventGen[C <: Command, S <: Status](cb: Long => C, sb: (Int, Long) => S): Gen[StateChangedEvent] =
    stateEventGen(cb, (r, st, _) => sb(r, st))

  def stateEventAnyStatusGen[C <: Command](cb: (Long, Int) => C): Gen[StateChangedEvent] =
    for {
      chatId       <- Gen.posNum[Long]
      (cmd, state) <- valueCommandAndAnyStateGen(cb)
    } yield StateChangedEvent(chatId, state, cmd)

  def stateEventAnyStatusGen[C <: Command](cb: Long => C): Gen[StateChangedEvent] =
    for {
      chatId       <- Gen.posNum[Long]
      (cmd, state) <- commandAndAnyStateGen(cb)
    } yield StateChangedEvent(chatId, state, cmd)

  def suspendedStateEventGen[C <: Command, S <: SuspendedStatus](
      cb: Long => C,
      sb: (Int, Long, Long) => S
  ): Gen[StateChangedEvent] =
    for {
      chatId       <- Gen.posNum[Long]
      (cmd, state) <- commandAndSuspendedStateGen(cb, sb)
    } yield StateChangedEvent(chatId, state, cmd)

}
