package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User.{apply => _, _}
import org.chepiov.tomodoro.algebras.{Statistic, Telegram, Tomodoro, Users}
import org.chepiov.tomodoro.programs.UserMessages._

class TomodoroInterpreter[F[_]: Logger: Monad](
    users: Users[F],
    statistic: Statistic[F],
    telegram: Telegram[F]
) extends Tomodoro[F] {
  import TomodoroInterpreter._

  override def getInfo: F[TUser] =
    for {
      _ <- Logger[F].debug("Received info request")
      r <- telegram.getMe
    } yield r

  override def handleUpdate(update: TUpdate): F[Unit] = {
    update match {
      case statsQuery(chatId)             => query(chatId, GetStats)
      case helpQuery(chatId)              => query(chatId, GetHelp)
      case stateQuery(chatId)             => query(chatId, GetState)
      case startCmd(chatId)               => advance(chatId, Continue)
      case pauseCmd(chatId)               => advance(chatId, Suspend)
      case resetCmd(chatId)               => advance(chatId, Reset)
      case skipCmd(chatId)                => advance(chatId, Skip)
      case settingsCmd(chatId)            => advance(chatId, SetSettings)
      case settingsValue((chatId, value)) => advance(chatId, SetSettingsValue.apply(_, value))
      case settingsCallbackQuery(chatId, callbackId, cmd) =>
        for {
          _ <- advance(chatId, _ => cmd)
          r <- telegram.answerCallbackQuery(TCallbackAnswer(callbackId))
        } yield r
      case statsCallbackQuery(chatId, callbackId, statsType) =>
        for {
          _     <- Logger[F].debug(s"[$chatId] Received stats callback query, type: $statsType")
          stats <- getStats(chatId, statsType)
          user  <- users.getOrCreateUser(chatId)
          _     <- user.stats(stats)
          r     <- telegram.answerCallbackQuery(TCallbackAnswer(callbackId))
        } yield r
      case _ => for (r <- Logger[F].warn(s"Unknown update: $update")) yield r
    }
  }

  override def setWebHook(updateUrl: String): F[Unit] = Monad[F].unit

  override def deleteWebHook(): F[Unit] = Monad[F].unit

  private def query(chatId: Long, query: UserInfoQuery): F[Unit] =
    for {
      _    <- Logger[F].debug(s"[$chatId] Received $query query")
      user <- users.getOrCreateUser(chatId)
      r    <- user.info(query)
    } yield r

  private def advance(chatId: Long, cmd: Long => UserCommand): F[Unit] =
    for {
      user    <- users.getOrCreateUser(chatId)
      command = cmd(now)
      _       <- Logger[F].debug(s"[$chatId] Received $command command")
      r       <- user.advance(cmd(now))
    } yield r

  private def getStats(chatId: Long, statsType: StatsType): F[UserStatsResult] =
    statsType match {
      case Log =>
        for {
          _   <- Logger[F].debug(s"[$chatId] Getting log statistic")
          log <- statistic.getLog(chatId, 0, 0)
          _   <- Logger[F].debug(s"[$chatId] Log: $log")
        } yield PushLog
      case LastDay =>
        for {
          _         <- Logger[F].debug(s"[$chatId] Getting completed last day statistic")
          completed <- statistic.getCompletedLastDay(chatId)
          _         <- Logger[F].debug(s"[$chatId] Completed last day: $completed")
        } yield PushCompletedLastDay
      case LastWeek =>
        for {
          _         <- Logger[F].debug(s"[$chatId] Getting completed last day statistic")
          completed <- statistic.getCompletedLastDay(chatId)
          _         <- Logger[F].debug(s"[$chatId] Completed last day: $completed")
        } yield PushCompletedLastWeek
      case LastMonth =>
        for {
          _         <- Logger[F].debug(s"[$chatId] Getting completed last day statistic")
          completed <- statistic.getCompletedLastDay(chatId)
          _         <- Logger[F].debug(s"[$chatId] Completed last day: $completed")
        } yield PushCompletedLastMonth
    }
}

case object TomodoroInterpreter {
  def apply[I[_]: Monad, F[_]: Logger: Monad](
      users: Users[F],
      statistic: Statistic[F],
      telegram: Telegram[F]
  ): I[Tomodoro[F]] =
    for {
      _ <- Monad[I].unit
      t = new TomodoroInterpreter[F](users, statistic, telegram)
    } yield t

  def apply[F[_]: Logger: Monad](
      users: Users[F],
      statistic: Statistic[F],
      telegram: Telegram[F]
  ): F[Tomodoro[F]] = apply[F, F](users, statistic, telegram)

  private def now: Long = OffsetDateTime.now().toEpochSecond

  private object helpQuery {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/help"))), None) => chatId.some
      case _                                                                 => none
    }
  }

  private object stateQuery {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/state"))), None) => chatId.some
      case _                                                                  => none
    }
  }

  private object startCmd {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/start"))), None)    => chatId.some
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/continue"))), None) => chatId.some
      case _                                                                     => none
    }
  }

  private object pauseCmd {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/pause"))), None) => chatId.some
      case _                                                                  => none
    }
  }

  private object resetCmd {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/reset"))), None) => chatId.some
      case _                                                                  => none
    }
  }

  private object skipCmd {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/skip"))), None) => chatId.some
      case _                                                                 => none
    }
  }

  private object settingsCmd {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/settings"))), None) => chatId.some
      case _                                                                     => none
    }
  }

  private object settingsValue {
    def unapply(update: TUpdate): Option[(Long, Int)] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some(value))), None) if value.forall(Character.isDigit) =>
        (chatId, value.toInt).some
      case _ => none
    }
  }

  private object statsQuery {
    def unapply(update: TUpdate): Option[Long] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some("/stats"))), None) => chatId.some
      case _                                                                  => none
    }
  }

  private object settingsCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, String, UserCommand)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data)))) =>
        val cmd = data match {
          case SettingsDurationData   => AwaitChangingDuration(now).some
          case SettingsLongBreakData  => AwaitChangingLongBreak(now).some
          case SettingsShortBreakData => AwaitChangingShortBreak(now).some
          case SettingsAmountData     => AwaitChangingAmount(now).some
          case _                      => none[UserCommand]
        }
        cmd.map(c => (chatId, callbackId, c))
      case _ => none
    }
  }

  private object statsCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, String, StatsType)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data)))) =>
        val statsType = data match {
          case StatsLogData           => Log.some
          case StatsCountPerDayData   => LastDay.some
          case StatsCountPerWeekData  => LastWeek.some
          case StatsCountPerMonthData => LastMonth.some
          case _                      => none[StatsType]
        }
        statsType.map(c => (chatId, callbackId, c))
      case _ => none
    }
  }

  sealed private trait StatsType extends Product with Serializable
  private case object Log        extends StatsType
  private case object LastDay    extends StatsType
  private case object LastWeek   extends StatsType
  private case object LastMonth  extends StatsType
}
