package org.chepiov.tomodoro.programs

import cats.data.State
import org.chepiov.tomodoro.algebras.Telegram.TSendMessage
import org.chepiov.tomodoro.algebras.User._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, OptionValues, PropSpec}

import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Any"))
class UserStateMachineSpec extends PropSpec with Matchers with PropertyChecks with OptionValues {
  import UserStateMachineSpec._

  private def advance(command: Command): State[UserState, Option[TSendMessage]] =
    UserStateMachine.advance(1L, command, MINUTES)

  private def toSeconds(duration: Int): Long = FiniteDuration(duration.toLong, MINUTES).toSeconds

  property("Command with invalid time must not change state") {
    forAll(invalidTimeCommandAndStateGen) {
      case (command, initial) =>
        whenever(command.time < initial.status.startTime) {
          val (state, answer) = advance(command).run(initial).value
          state shouldBe initial
          answer shouldBe empty
        }
    }
  }

  property("Command with invalid time must not change suspended state") {
    forAll(invalidTimeCommandAndSuspendedStateGen) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe empty
    }
  }

  property("Continue must change WaitingWork to Working") {
    forAll(commandAndStateGen(Continue, WaitingWork)) {
      case (command, initial) =>
        whenever(initial.status.remaining > 0) {
          val (state, answer) = advance(command).run(initial).value
          state.status shouldBe a[Working]
          state.settings shouldBe initial.settings
          answer shouldBe 'defined

          state.status.asInstanceOf[Working].endTime shouldBe (command.time + toSeconds(state.settings.duration))
          state.status.startTime shouldBe command.time

          state.status.remaining shouldBe (initial.status.remaining - 1)
          state.status.remaining should (be <= state.settings.amount and be >= 0)
        }
    }
  }

  property("Continue must change WaitingBreak to Breaking") {
    forAll(commandAndStateGen(Continue, WaitingBreak)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[Breaking]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        if (state.status.remaining == 0)
          state.status.asInstanceOf[Breaking].endTime shouldBe (command.time + toSeconds(state.settings.longBreak))
        else
          state.status.asInstanceOf[Breaking].endTime shouldBe (command.time + toSeconds(state.settings.shortBreak))
        state.status.startTime shouldBe command.time

        state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be >= 0)
    }

  }

  property("Continue must change WorkSuspended to Working") {
    forAll(commandAndSuspendedStateGen(Continue, WorkSuspended)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[Working]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        val initialStatus = initial.status.asInstanceOf[WorkSuspended]
        val worked        = initialStatus.suspend - initialStatus.startTime
        val end           = command.time + (toSeconds(state.settings.duration) - worked)
        state.status.asInstanceOf[Working].endTime shouldBe end
        state.status.startTime shouldBe command.time

        state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be >= 0)
    }
  }

  property("Continue must change BreakSuspended to Breaking") {
    forAll(commandAndSuspendedStateGen(Continue, BreakSuspended)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[Breaking]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        val initialStatus = initial.status.asInstanceOf[BreakSuspended]
        val suspended     = initialStatus.suspend - initialStatus.startTime
        val end =
          if (state.status.remaining == 0)
            command.time + toSeconds(state.settings.longBreak) - suspended
          else
            command.time + toSeconds(state.settings.shortBreak) - suspended
        state.status.asInstanceOf[Breaking].endTime shouldBe end
        state.status.startTime shouldBe command.time

        state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be >= 0)
    }
  }

  property("Continue for inappropriate state must change nothing") {
    forAll(commandAndFiniteStateGen(Continue)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe 'defined
    }
  }

  property("Continue for illegal state must change nothing") {
    forAll(continueAndWaitingWorkWithoutRemaining) {
      case (command, initial) =>
        whenever(initial.status.remaining == 0) {
          val (state, answer) = advance(command).run(initial).value
          state shouldBe initial
          answer shouldBe empty
        }
    }
  }

  property("Finish must change Working to WaitingBreak") {
    forAll(commandAndStateGen(Finish, Working)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[WaitingBreak]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        state.status.startTime shouldBe command.time

        state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be >= 0)
    }
  }

  property("Finish must change Breaking to WaitingWork") {
    forAll(commandAndStateGen(Finish, Breaking)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[WaitingWork]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        state.status.startTime shouldBe command.time

        if (initial.status.remaining == 0)
          state.status.remaining shouldBe state.settings.amount
        else
          state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be > 0)
    }
  }

  property("Finish must not change nothing for WorkSuspended") {
    forAll(commandAndSuspendedStateGen(Finish, WorkSuspended)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe empty
    }
  }

  property("Finish must not change nothing for BreakSuspended") {
    forAll(commandAndSuspendedStateGen(Finish, BreakSuspended)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe empty
    }
  }

  property("Finish must not change nothing for WaitingWork") {
    forAll(commandAndStateGen(Finish, WaitingWork)) {
      case (command, initial) =>
        whenever(initial.status.remaining > 0) {
          val (state, answer) = advance(command).run(initial).value
          state shouldBe initial
          answer shouldBe empty
        }
    }
  }

  property("Finish must not change nothing for WaitingBreak") {
    forAll(commandAndStateGen(Finish, WaitingBreak)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe empty
    }
  }

  property("Suspend must change Working to WorkSuspended") {
    forAll(commandAndStateGen(Suspend, Working)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[WorkSuspended]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        state.status.asInstanceOf[WorkSuspended].suspend shouldBe command.time
        state.status.startTime shouldBe initial.status.startTime

        state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be >= 0)
    }
  }

  property("Suspend must change Breaking to BreakSuspended") {
    forAll(commandAndStateGen(Suspend, Breaking)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state.status shouldBe a[BreakSuspended]
        state.settings shouldBe initial.settings
        answer shouldBe 'defined

        state.status.asInstanceOf[BreakSuspended].suspend shouldBe command.time
        state.status.startTime shouldBe initial.status.startTime

        state.status.remaining shouldBe initial.status.remaining
        state.status.remaining should (be <= state.settings.amount and be >= 0)
    }
  }

  property("Suspend must not change nothing for WorkSuspended") {
    forAll(commandAndSuspendedStateGen(Suspend, WorkSuspended)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe 'defined
    }
  }

  property("Suspend must not change nothing for BreakSuspended") {
    forAll(commandAndSuspendedStateGen(Suspend, BreakSuspended)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe 'defined
    }
  }

  property("Suspend must not change nothing for WaitingWork") {
    forAll(commandAndStateGen(Suspend, WaitingWork)) {
      case (command, initial) =>
        whenever(initial.status.remaining > 0) {
          val (state, answer) = advance(command).run(initial).value
          state shouldBe initial
          answer shouldBe 'defined
        }
    }
  }

  property("Suspend must not change nothing for WaitingBreak") {
    forAll(commandAndStateGen(Suspend, WaitingBreak)) {
      case (command, initial) =>
        val (state, answer) = advance(command).run(initial).value
        state shouldBe initial
        answer shouldBe 'defined
    }
  }

  property("SetSettings must change nothing") {
    forAll(commandAndAnyStateGen(SetSettings)) {
      case (command, initial) =>
        whenever(
          (initial.status.isInstanceOf[WaitingWork] && initial.status.remaining > 0) || !initial.status
            .isInstanceOf[WaitingWork]
        ) {
          val (state, answer) = advance(command).run(initial).value
          state shouldBe initial
          answer shouldBe 'defined
        }
    }
  }

  property("Stop must change state to WaitingForWork") {
    forAll(commandAndAnyStateGen(Reset)) {
      case (command, initial) =>
        whenever(
          (initial.status.isInstanceOf[WaitingWork] && initial.status.remaining > 0) || !initial.status
            .isInstanceOf[WaitingWork]
        ) {
          val (state, answer) = advance(command).run(initial).value
          state.status shouldBe a[WaitingWork]
          state.settings shouldBe initial.settings
          answer shouldBe 'defined

          state.status.startTime shouldBe command.time

          state.status.remaining shouldBe state.settings.amount
        }
    }
  }
}

case object UserStateMachineSpec {

  def commandGen[A <: Command](f: (Long, UserSettings) => A): Gen[A] =
    for {
      time     <- Gen.posNum[Long]
      settings <- settingsGen
    } yield f(time, settings)

  def commandGen[A <: Command](f: Long => A): Gen[A] =
    commandGen((t, _) => f(t))

  def statusGen[A <: Status](f: (Int, Long, Long) => A)(startMax: Long, amount: Int): Gen[A] =
    for {
      remaining <- Gen.choose(0, amount)
      start     <- Gen.choose(0, startMax)
      end       <- Gen.choose(start, Long.MaxValue)
    } yield f(remaining, start, end)

  def suspendedStatusGen[A <: SuspendedStatus](f: (Int, Long, Long) => A)(startMax: Long, amount: Int): Gen[A] =
    for {
      remaining <- Gen.choose(0, amount)
      start     <- Gen.choose(0, startMax)
      suspend   <- Gen.choose(start, startMax)
    } yield f(remaining, start, suspend)

  def statusGen[A <: Status](f: (Int, Long) => A)(startMax: Long, amount: Int): Gen[A] =
    statusGen((r, s, _) => f(r, s))(startMax, amount)

  def suspendedStatusGen[A <: SuspendedStatus](f: (Int, Long) => A)(startMax: Long, amount: Int): Gen[A] =
    suspendedStatusGen((r, s, _) => f(r, s))(startMax, amount)

  val settingsGen: Gen[UserSettings] =
    for {
      duration   <- Gen.posNum[Int]
      shortBreak <- Gen.posNum[Int]
      longBreak  <- Gen.posNum[Int]
      amount     <- Gen.posNum[Int]
    } yield UserSettings(duration, shortBreak, longBreak, amount)

  val anyCommandGen: Gen[Command] =
    for {
      command <- Gen.oneOf(
                  commandGen(SetSettings),
                  commandGen(Continue),
                  commandGen(Finish),
                  commandGen(Suspend),
                  commandGen(Reset)
                )
    } yield command

  def anyStateGen(startMax: Long): Gen[UserState] =
    for {
      settings <- settingsGen
      status <- Gen.oneOf(
                 statusGen(WaitingWork)(startMax, settings.amount),
                 statusGen(WaitingBreak)(startMax, settings.amount),
                 statusGen(Working)(startMax, settings.amount),
                 statusGen(Breaking)(startMax, settings.amount),
                 suspendedStatusGen(WorkSuspended)(startMax, settings.amount),
                 suspendedStatusGen(BreakSuspended)(startMax, settings.amount)
               )
    } yield UserState(settings, status)

  def finiteStateGen(startMax: Long): Gen[UserState] = {
    for {
      settings <- settingsGen
      status <- Gen.oneOf(
                 statusGen(Working)(startMax, settings.amount),
                 statusGen(Breaking)(startMax, settings.amount)
               )
    } yield UserState(settings, status)
  }

  def stateGen[A <: Status](startMax: Long, f: (Int, Long, Long) => A): Gen[UserState] =
    for {
      settings <- settingsGen
      status   <- statusGen(f)(startMax, settings.amount)
    } yield UserState(settings, status)

  def suspendedStateGen[A <: SuspendedStatus](startMax: Long, f: (Int, Long, Long) => A): Gen[UserState] =
    for {
      settings <- settingsGen
      status   <- suspendedStatusGen(f)(startMax, settings.amount)
    } yield UserState(settings, status)

  def stateGen[A <: Status](startMax: Long, f: (Int, Long) => A): Gen[UserState] =
    stateGen(startMax, (r, s, _) => f(r, s))

  def suspendedStateGen[A <: SuspendedStatus](startMax: Long, f: (Int, Long) => A): Gen[UserState] =
    suspendedStateGen(startMax, (r, s, _) => f(r, s))

  val anyCommandAndStateGen: Gen[(Command, UserState)] =
    for {
      cmd   <- anyCommandGen
      state <- anyStateGen(cmd.time)
    } yield (cmd, state)

  val invalidTimeCommandAndStateGen: Gen[(Command, UserState)] =
    for {
      cmd   <- anyCommandGen
      state <- anyStateGen(Long.MaxValue).filter(_.status.startTime > cmd.time)
    } yield (cmd, state)

  val invalidTimeCommandAndSuspendedStateGen: Gen[(Command, UserState)] =
    for {
      cmd <- anyCommandGen
      state <- Gen
                .oneOf(
                  stateGen(cmd.time, WorkSuspended),
                  stateGen(cmd.time, BreakSuspended)
                )
                .filter(_.status.asInstanceOf[SuspendedStatus].suspend > cmd.time)
    } yield (cmd, state)

  def commandAndFiniteStateGen[C <: Command](cb: (Long, UserSettings) => C): Gen[(Command, UserState)] =
    for {
      cmd   <- commandGen(cb)
      state <- finiteStateGen(cmd.time)
    } yield (cmd, state)

  def commandAndFiniteStateGen[C <: Command](cb: Long => C): Gen[(Command, UserState)] =
    commandAndFiniteStateGen((t, _) => cb(t))

  def commandAndStateGen[C <: Command, S <: Status](
      cb: (Long, UserSettings) => C,
      sb: (Int, Long, Long) => S
  ): Gen[(Command, UserState)] =
    for {
      cmd   <- commandGen(cb)
      state <- stateGen(cmd.time, sb)
    } yield (cmd, state)

  def commandAndSuspendedStateGen[C <: Command, S <: SuspendedStatus](
      cb: (Long, UserSettings) => C,
      sb: (Int, Long, Long) => S
  ): Gen[(Command, UserState)] =
    for {
      cmd   <- commandGen(cb)
      state <- suspendedStateGen(cmd.time, sb)
    } yield (cmd, state)

  def commandAndStateGen[C <: Command, S <: Status](
      cb: Long => C,
      sb: (Int, Long, Long) => S
  ): Gen[(Command, UserState)] =
    commandAndStateGen((t, _) => cb(t), sb)

  def commandAndSuspendedStateGen[C <: Command, S <: SuspendedStatus](
      cb: Long => C,
      sb: (Int, Long, Long) => S
  ): Gen[(Command, UserState)] =
    commandAndSuspendedStateGen((t, _) => cb(t), sb)

  def commandAndStateGen[C <: Command, S <: Status](
      cb: Long => C,
      sb: (Int, Long) => S
  ): Gen[(Command, UserState)] =
    commandAndStateGen((t, _) => cb(t), (r, s, _) => sb(r, s))

  def commandAndAnyStateGen[C <: Command](cb: (Long, UserSettings) => C): Gen[(Command, UserState)] =
    for {
      cmd   <- commandGen(cb)
      state <- anyStateGen(cmd.time)
    } yield (cmd, state)

  def commandAndAnyStateGen[C <: Command](cb: Long => C): Gen[(Command, UserState)] =
    commandAndAnyStateGen((t, _) => cb(t))

  val continueAndWaitingWorkWithoutRemaining: Gen[(Command, UserState)] =
    for {
      cmd      <- commandGen(Continue)
      settings <- settingsGen
      status   <- statusGen(WaitingWork)(cmd.time, 0)
    } yield (cmd, UserState(settings, status))
}
