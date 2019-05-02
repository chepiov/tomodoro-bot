package org.chepiov.tomodoro.interpreters

import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{Clock, IO, Timer}
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.algebras._
import org.chepiov.tomodoro.programs.UserMessageData.SettingsData._
import org.chepiov.tomodoro.programs.UserMessageData.StatsData._
import org.chepiov.tomodoro.programs.UserMessages
import org.scalatest.{Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class TomodoroInterpreterSpec extends WordSpecLike with Matchers {
  import TomodoroInterpreterSpec._

  "Tomodoro" should {
    "handle queries correctly" in {
      val program = for {
        userState                   <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
        user                        = new UserIO(userState)
        users                       = new UsersIO(user)
        statisticState              <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
        statistic                   = new StatisticIO(statisticState)
        telegramState               <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
        telegram                    = new TelegramIO(telegramState)
        implicit0(timer: Timer[IO]) = constTimer
        tomodoro                    <- TomodoroInterpreter[IO](users, statistic, telegram)
        _                           <- tomodoro.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "state".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(2L, TMessage(2L, TChat(1L), "stats".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(3L, TMessage(3L, TChat(1L), "help".some).some, none))
        (_, queries)                <- userState.get
      } yield queries shouldBe List(GetState, GetStats, GetHelp)
      program.unsafeRunSync()
    }
  }

  it should {
    "handle commands correctly" in {
      val program = for {
        userState                   <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
        user                        = new UserIO(userState)
        users                       = new UsersIO(user)
        statisticState              <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
        statistic                   = new StatisticIO(statisticState)
        telegramState               <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
        telegram                    = new TelegramIO(telegramState)
        implicit0(timer: Timer[IO]) = constTimer
        tomodoro                    <- TomodoroInterpreter[IO](users, statistic, telegram)
        _                           <- tomodoro.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "continue".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(2L, TMessage(2L, TChat(1L), "pause".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(3L, TMessage(3L, TChat(1L), "reset".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(4L, TMessage(4L, TChat(1L), "skip".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(5L, TMessage(5L, TChat(1L), "settings".some).some, none))
        _                           <- tomodoro.handleUpdate(TUpdate(6L, TMessage(6L, TChat(1L), "42".some).some, none))
        (commands, _)               <- userState.get

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
        userState                       <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
        user                            = new UserIO(userState)
        users                           = new UsersIO(user)
        statisticState                  <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
        statistic                       = new StatisticIO(statisticState)
        telegramState                   <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
        telegram                        = new TelegramIO(telegramState)
        implicit0(timer: Timer[IO])     = constTimer
        tomodoro                        <- TomodoroInterpreter[IO](users, statistic, telegram)
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
        (commands, _)                   <- userState.get
        TelegramState(_, answers, _, _) <- telegramState.get
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
        userState                         <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
        user                              = new UserIO(userState)
        users                             = new UsersIO(user)
        statisticState                    <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
        statistic                         = new StatisticIO(statisticState)
        telegramState                     <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
        telegram                          = new TelegramIO(telegramState)
        implicit0(timer: Timer[IO])       = constTimer
        tomodoro                          <- TomodoroInterpreter[IO](users, statistic, telegram)
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
        StatisticState(as, lds, lws, lms) <- statisticState.get
        TelegramState(_, answers, _, _)   <- telegramState.get
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
        userState                       <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
        user                            = new UserIO(userState)
        users                           = new UsersIO(user)
        statisticState                  <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
        statistic                       = new StatisticIO(statisticState)
        telegramState                   <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
        telegram                        = new TelegramIO(telegramState)
        implicit0(timer: Timer[IO])     = constTimer
        tomodoro                        <- TomodoroInterpreter[IO](users, statistic, telegram)
        user                            = TUser(1L, isBot = false, "test", none, none)
        message                         = TMessage(1L, TChat(1L), none)
        logQuery                        = TCallbackQuery("id1", user, message.some, s"${StatsLogData.entryName}:1".some)
        _                               <- tomodoro.handleUpdate(TUpdate(1L, none, logQuery.some))
        StatisticState(as, _, _, _)     <- statisticState.get
        TelegramState(_, answers, _, _) <- telegramState.get
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
        userState                    <- Ref.of[IO, (List[UserCommand], List[UserInfoQuery])]((List(), List()))
        user                         = new UserIO(userState)
        users                        = new UsersIO(user)
        statisticState               <- Ref.of[IO, StatisticState](StatisticState(List(), List(), List(), List()))
        statistic                    = new StatisticIO(statisticState)
        telegramState                <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
        telegram                     = new TelegramIO(telegramState)
        implicit0(timer: Timer[IO])  = constTimer
        tomodoro                     <- TomodoroInterpreter[IO](users, statistic, telegram)
        _                            <- tomodoro.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "unknown".some).some, none))
        TelegramState(send, _, _, _) <- telegramState.get
      } yield send shouldBe List(UserMessages.unknownMsg(1L))
      program.unsafeRunSync()
    }
  }
}

case object TomodoroInterpreterSpec {

  object constTimer extends Timer[IO] {
    private val sc = Executors.newScheduledThreadPool(3)
    private val ec = ExecutionContext.fromExecutor(sc)

    override val clock: Clock[IO] =
      new Clock[IO] {
        override def realTime(unit: TimeUnit): IO[Long]  = 0L.pure[IO]
        override def monotonic(unit: TimeUnit): IO[Long] = 0L.pure[IO]
      }

    override def sleep(timespan: FiniteDuration): IO[Unit] =
      IO.cancelable { cb =>
        val tick = new Runnable {
          def run(): Unit = ec.execute(() => cb(Right(())))
        }
        val f = sc.schedule(tick, timespan.length, timespan.unit)
        IO(f.cancel(false)).void
      }
  }

  class UserIO(state: Ref[IO, (List[UserCommand], List[UserInfoQuery])]) extends User[IO] {
    def advance(cmd: User.UserCommand): IO[Unit]  = state.update { case (cs, qs) => (cs :+ cmd, qs) }
    def info(query: User.UserInfoQuery): IO[Unit] = state.update { case (cs, qs) => (cs, qs :+ query) }
  }

  class UsersIO(user: User[IO]) extends Users[IO] {
    def getOrCreateUser(chatId: Long): IO[User[IO]] = user.pure[IO]
  }

  class StatisticIO(state: Ref[IO, StatisticState]) extends Statistic[IO] {
    def sendActivity(chatId: Long, page: Int, messageId: Option[Long]): IO[Unit] =
      state.update(_ addActivity ((chatId, page, messageId)))
    def sendCompletedLastDay(chatId: Long): IO[Unit]   = state.update(_ addLastDay chatId)
    def sendCompletedLastWeek(chatId: Long): IO[Unit]  = state.update(_ addLastWeek chatId)
    def sendCompletedLastMonth(chatId: Long): IO[Unit] = state.update(_ addLastMonth chatId)
  }

  type Activity = (Long, Int, Option[Long])
  case class StatisticState(
      activity: List[Activity],
      lastDay: List[Long],
      lastWeek: List[Long],
      lastMonth: List[Long]
  ) { self =>
    def addActivity(a: Activity): StatisticState   = self.copy(activity = activity :+ a)
    def addLastDay(chatId: Long): StatisticState   = self.copy(lastDay = lastDay :+ chatId)
    def addLastWeek(chatId: Long): StatisticState  = self.copy(lastWeek = lastWeek :+ chatId)
    def addLastMonth(chatId: Long): StatisticState = self.copy(lastMonth = lastMonth :+ chatId)
  }

  case class TelegramState(
      send: List[TSendMessage],
      answer: List[TCallbackAnswer],
      edit: List[TEditMessage],
      me: Int
  ) { self =>
    def addSend(m: TSendMessage): TelegramState      = self.copy(send = send :+ m)
    def addAnswer(a: TCallbackAnswer): TelegramState = self.copy(answer = answer :+ a)
    def addEdit(m: TEditMessage): TelegramState      = self.copy(edit = edit :+ m)
    def addMe: TelegramState                         = self.copy(me = me + 1)
  }

  class TelegramIO(state: Ref[IO, TelegramState]) extends Telegram[IO] {
    def sendMessage(message: TSendMessage): IO[Unit]           = state.update(_ addSend message)
    def answerCallbackQuery(answer: TCallbackAnswer): IO[Unit] = state.update(_ addAnswer answer)
    def editMessageText(message: TEditMessage): IO[Unit]       = state.update(_ addEdit message)
    def getMe: IO[TUser]                                       = state.update(_.addMe).as(TUser(1, isBot = true, "tomodoro", none, none))
  }
}
