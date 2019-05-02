package org.chepiov.tomodoro.interpreters

import cats.effect.concurrent.Ref
import cats.effect.{IO, Timer}
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras._
import org.chepiov.tomodoro.programs.UserMessageData.SettingsData._
import org.chepiov.tomodoro.programs.UserMessageData.StatsData._
import org.chepiov.tomodoro.programs.UserMessages
import org.scalatest.{Matchers, WordSpecLike}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class TomodoroInterpreterSpec extends WordSpecLike with Matchers with InterpreterSpec {

  private def create(): IO[(Ref[IO, UserState], Ref[IO, StatisticState], Ref[IO, TelegramState], Tomodoro[IO])] =
    for {
      userState                   <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
      user                        = new UserIO(userState)
      users                       = new UsersIO(user)
      statisticState              <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
      statistic                   = new StatisticIO(statisticState)
      telegramState               <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
      telegram                    = new TelegramIO(telegramState)
      implicit0(timer: Timer[IO]) = constTimer
      tomodoro                    <- TomodoroInterpreter[IO](users, statistic, telegram)
    } yield (userState, statisticState, telegramState, tomodoro)

  "Tomodoro" should {
    "handle queries correctly" in {
      val program = for {
        (us, _, _, tomodoro) <- create()
        _                    <- tomodoro.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "state".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(2L, TMessage(2L, TChat(1L), "stats".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(3L, TMessage(3L, TChat(1L), "help".some).some, none))
        (_, queries)         <- us.get
      } yield queries shouldBe List(GetState, GetStats, GetHelp)
      program.unsafeRunSync()
    }
  }

  it should {
    "handle commands correctly" in {
      val program = for {
        (us, _, _, tomodoro) <- create()
        _                    <- tomodoro.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "continue".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(2L, TMessage(2L, TChat(1L), "pause".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(3L, TMessage(3L, TChat(1L), "reset".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(4L, TMessage(4L, TChat(1L), "skip".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(5L, TMessage(5L, TChat(1L), "settings".some).some, none))
        _                    <- tomodoro.handleUpdate(TUpdate(6L, TMessage(6L, TChat(1L), "42".some).some, none))
        (commands, _)        <- us.get

      } yield
        commands shouldBe List(
          Continue(0L),
          Suspend(0L),
          Reset(0L),
          Skip(0L),
          SetSettings(0L),
          SetSettingsValue(0L, 42)
        )
      program.unsafeRunSync()
    }
  }

  it should {
    "handle settings callback query correctly" in {
      val program = for {
        (us, _, ts, tomodoro)           <- create()
        user                            = TUser(1L, isBot = false, "test", none, none)
        message                         = TMessage(1L, TChat(1L), none)
        durationQuery                   = TCallbackQuery("id1", user, message.some, SettingsDurationData.entryName.some)
        shortQuery                      = TCallbackQuery("id2", user, message.some, SettingsShortBreakData.entryName.some)
        longQuery                       = TCallbackQuery("id3", user, message.some, SettingsLongBreakData.entryName.some)
        amountQuery                     = TCallbackQuery("id4", user, message.some, SettingsAmountData.entryName.some)
        _                               <- tomodoro.handleUpdate(TUpdate(1L, none, durationQuery.some))
        _                               <- tomodoro.handleUpdate(TUpdate(2L, none, shortQuery.some))
        _                               <- tomodoro.handleUpdate(TUpdate(3L, none, longQuery.some))
        _                               <- tomodoro.handleUpdate(TUpdate(4L, none, amountQuery.some))
        (commands, _)                   <- us.get
        TelegramState(_, answers, _, _) <- ts.get
      } yield {
        commands shouldBe List(
          AwaitChangingDuration(0L),
          AwaitChangingShortBreak(0L),
          AwaitChangingLongBreak(0L),
          AwaitChangingAmount(0L)
        )

        answers shouldBe List(
          TCallbackAnswer("id1"),
          TCallbackAnswer("id2"),
          TCallbackAnswer("id3"),
          TCallbackAnswer("id4")
        )
      }
      program.unsafeRunSync()
    }
  }

  it should {
    "handle stats callback query correctly" in {
      val program = for {
        (_, ss, ts, tomodoro)             <- create()
        user                              = TUser(1L, isBot = false, "test", none, none)
        message                           = TMessage(1L, TChat(1L), none)
        logQuery                          = TCallbackQuery("id1", user, message.some, StatsLogData.entryName.some)
        perDayQuery                       = TCallbackQuery("id2", user, message.some, StatsCountPerDayData.entryName.some)
        perWeekQuery                      = TCallbackQuery("id3", user, message.some, StatsCountPerWeekData.entryName.some)
        perMonthQuery                     = TCallbackQuery("id4", user, message.some, StatsCountPerMonthData.entryName.some)
        _                                 <- tomodoro.handleUpdate(TUpdate(1L, none, logQuery.some))
        _                                 <- tomodoro.handleUpdate(TUpdate(2L, none, perDayQuery.some))
        _                                 <- tomodoro.handleUpdate(TUpdate(3L, none, perWeekQuery.some))
        _                                 <- tomodoro.handleUpdate(TUpdate(4L, none, perMonthQuery.some))
        StatisticState(as, lds, lws, lms) <- ss.get
        TelegramState(_, answers, _, _)   <- ts.get
      } yield {
        as shouldBe List((1L, 0, none))
        lds shouldBe List(1L)
        lws shouldBe List(1L)
        lms shouldBe List(1L)

        answers shouldBe List(
          TCallbackAnswer("id1"),
          TCallbackAnswer("id2"),
          TCallbackAnswer("id3"),
          TCallbackAnswer("id4")
        )
      }
      program.unsafeRunSync()
    }
  }

  it should {
    "handle log callback query correctly" in {
      val program = for {
        (_, ss, ts, tomodoro)           <- create()
        user                            = TUser(1L, isBot = false, "test", none, none)
        message                         = TMessage(1L, TChat(1L), none)
        logQuery                        = TCallbackQuery("id1", user, message.some, s"${StatsLogData.entryName}:1".some)
        _                               <- tomodoro.handleUpdate(TUpdate(1L, none, logQuery.some))
        StatisticState(as, _, _, _)     <- ss.get
        TelegramState(_, answers, _, _) <- ts.get
      } yield {
        as shouldBe List((1L, 1, 1L.some))

        answers shouldBe List(
          TCallbackAnswer("id1")
        )
      }
      program.unsafeRunSync()
    }
  }

  it should {
    "handle unknown message correctly" in {
      val program = for {
        (_, _, ts, tomodoro)         <- create()
        _                            <- tomodoro.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "unknown".some).some, none))
        TelegramState(send, _, _, _) <- ts.get
      } yield send shouldBe List(UserMessages.unknownMsg(1L))
      program.unsafeRunSync()
    }
  }
}