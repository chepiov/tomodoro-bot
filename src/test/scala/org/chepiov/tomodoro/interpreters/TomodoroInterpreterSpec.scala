package org.chepiov.tomodoro.interpreters

import cats.effect.IO
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Telegram.{TChat, TMessage, TUpdate}
import org.chepiov.tomodoro.algebras._
import org.scalatest.{Matchers, WordSpecLike}

class TomodoroInterpreterSpec extends WordSpecLike with Matchers {
  import TomodoroInterpreterSpec._

  def tomodoro: IO[Tomodoro[IO]] =
    for {
      users     <- UsersIO()
      statistic <- StatisticIO()
      telegram  <- TelegramIO()
      tomodoro  <- TomodoroInterpreter[IO](users, statistic, telegram)
    } yield tomodoro

  "Tomodoro" should {
    "handle 'state' command correctly" in {
      val program = for {
        t <- tomodoro
        r <- t.handleUpdate(TUpdate(1L, TMessage(1L, TChat(1L), "state".some).some, none))
      } yield r
      //program.unsafeRunSync()
      println(program)
    }
  }
}

case object TomodoroInterpreterSpec {

  class UserIO extends User[IO] {
    override def advance(cmd: User.UserCommand): IO[Unit] = ???
    override def info(query: User.UserInfoQuery): IO[Unit] = ???
  }

  class UsersIO() extends Users[IO] {
    override def getOrCreateUser(chatId: Long): IO[User[IO]] = ???
  }

  object UsersIO {
    def apply(): IO[UsersIO] = IO(new UsersIO())
  }

  class StatisticIO extends Statistic[IO] {

    override def sendActivity(chatId: Long, page: Int, messageId: Option[Long]): IO[Unit] = ???

    override def sendCompletedLastDay(chatId: Long): IO[Unit] = ???

    override def sendCompletedLastWeek(chatId: Long): IO[Unit] = ???

    override def sendCompletedLastMonth(chatId: Long): IO[Unit] = ???
  }

  object StatisticIO {
    def apply(): IO[StatisticIO] = IO(new StatisticIO())
  }

  class TelegramIO extends Telegram[IO] {
    override def sendMessage(message: Telegram.TSendMessage): IO[Unit]           = ???
    override def answerCallbackQuery(answer: Telegram.TCallbackAnswer): IO[Unit] = ???
    override def editMessageText(message: Telegram.TEditMessage): IO[Unit]       = ???
    override def getMe: IO[Telegram.TUser]                                       = ???
  }

  object TelegramIO {
    def apply(): IO[TelegramIO] = IO(new TelegramIO())
  }
}
