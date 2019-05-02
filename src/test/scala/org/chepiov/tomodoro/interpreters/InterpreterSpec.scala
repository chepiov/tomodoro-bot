package org.chepiov.tomodoro.interpreters

import java.util.concurrent.Executors

import cats.effect.{Clock, IO, Timer}
import cats.effect.concurrent.Ref
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram.{TCallbackAnswer, TEditMessage, TSendMessage, TUser}
import org.chepiov.tomodoro.algebras.User.{UserCommand, UserInfoQuery}
import org.chepiov.tomodoro.algebras.{Statistic, Telegram, User, Users}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, TimeUnit}

trait InterpreterSpec {

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

  type UserState = (List[UserCommand], List[UserInfoQuery])
  class UserIO(state: Ref[IO, UserState]) extends User[IO] {
    def advance(cmd: User.UserCommand): IO[Unit]  = state.update { case (cs, qs) => (cs :+ cmd, qs) }
    def info(query: User.UserInfoQuery): IO[Unit] = state.update { case (cs, qs) => (cs, qs :+ query) }
  }

  class UsersIO(user: User[IO]) extends Users[IO] {
    def getOrCreateUser(chatId: Long): IO[User[IO]] = user.pure[IO]
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

  class StatisticIO(state: Ref[IO, StatisticState]) extends Statistic[IO] {
    def sendActivity(chatId: Long, page: Int, messageId: Option[Long]): IO[Unit] =
      state.update(_ addActivity ((chatId, page, messageId)))
    def sendCompletedLastDay(chatId: Long): IO[Unit]   = state.update(_ addLastDay chatId)
    def sendCompletedLastWeek(chatId: Long): IO[Unit]  = state.update(_ addLastWeek chatId)
    def sendCompletedLastMonth(chatId: Long): IO[Unit] = state.update(_ addLastMonth chatId)
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
