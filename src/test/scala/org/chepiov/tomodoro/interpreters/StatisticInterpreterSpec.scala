package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.syntax.option._
import org.chepiov.tomodoro.algebras.Repository.ActivityDescriptor._
import org.chepiov.tomodoro.algebras.Repository.ActivityLog
import org.chepiov.tomodoro.algebras.Statistic
import org.chepiov.tomodoro.programs.UserMessages._
import org.scalatest.{Matchers, WordSpecLike}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class StatisticInterpreterSpec extends WordSpecLike with Matchers with InterpreterSpec {

  private def create(): IO[(Ref[IO, TelegramState], Ref[IO, RepositoryState], Statistic[IO])] =
    for {
      telegramState   <- Ref.of[IO, TelegramState](TelegramState(List(), List(), List(), 0))
      telegram        = new TelegramIO(telegramState)
      repositoryState <- Ref.of[IO, RepositoryState](RepositoryState(List()))
      repository      = new RepositoryIO(repositoryState)
      statistic       <- StatisticInterpreter[IO](telegram, repository)
    } yield (telegramState, repositoryState, statistic)

  "Statistic" should {
    "send activities correctly" in {
      val program = for {
        (ts, rs, statistic)             <- create()
        logs                            = ActivityLog(1L, OffsetDateTime.now(), CycleReset, "test") :: Nil
        _                               <- rs.set(RepositoryState(logs))
        _                               <- statistic.sendActivity(1L, 0, none)
        _                               <- statistic.sendActivity(1L, 0, 1L.some)
        TelegramState(send, _, edit, _) <- ts.get
      } yield {
        send shouldBe List(logsMsg(1L, 0, logs))
        edit shouldBe List(logsEditMsg(1L, 1L, 0, logs))
      }
      program.unsafeRunSync()
    }
  }

  it should {
    "send completed count correctly" in {
      val program = for {
        (ts, rs, statistic) <- create()
        now                 = OffsetDateTime.now()
        logs = List(
          ActivityLog(1L, now, CycleReset, "today"),
          ActivityLog(1L, now.minusDays(2), CycleReset, "lastWeek"),
          ActivityLog(1L, now.minusDays(8), CycleReset, "lastMonth")
        )
        _                            <- rs.set(RepositoryState(logs))
        _                            <- statistic.sendCompletedLastDay(1L)
        _                            <- statistic.sendCompletedLastWeek(1L)
        _                            <- statistic.sendCompletedLastMonth(1L)
        TelegramState(send, _, _, _) <- ts.get
      } yield {
        send shouldBe List(
          completedLastDayMsg(1L, 1),
          completedLastWeekMsg(1L, 2),
          completedLastMonthMsg(1L, 3)
        )
      }
      program.unsafeRunSync()
    }
  }
}
