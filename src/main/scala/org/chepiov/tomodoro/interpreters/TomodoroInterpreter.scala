package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.Monad
import cats.effect.Sync
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User.{apply => _, _}
import org.chepiov.tomodoro.algebras.{Statistic, Telegram, Tomodoro, Users}
import org.chepiov.tomodoro.programs.UserMessages._

import scala.util.Try

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
      case commandOrQuery(chatId, text) if stateSynonyms.contains(text)    => query(chatId, GetState)
      case commandOrQuery(chatId, text) if statsSynonyms.contains(text)    => query(chatId, GetStats)
      case commandOrQuery(chatId, text) if helpSynonyms.contains(text)     => query(chatId, GetHelp)
      case commandOrQuery(chatId, text) if continueSynonyms.contains(text) => advance(chatId, Continue)
      case commandOrQuery(chatId, text) if pauseSynonyms.contains(text)    => advance(chatId, Suspend)
      case commandOrQuery(chatId, text) if resetSynonyms.contains(text)    => advance(chatId, Reset)
      case commandOrQuery(chatId, text) if skipSynonyms.contains(text)     => advance(chatId, Skip)
      case commandOrQuery(chatId, text) if settingsSynonyms.contains(text) => advance(chatId, SetSettings)
      case settingsValue((chatId, value))                                  => advance(chatId, SetSettingsValue.apply(_, value))
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
      case statsLogCallbackQuery(chatId, callbackId, page) =>
        for {
          _     <- Logger[F].debug(s"[$chatId] Received log page callback query, page: $page")
          stats <- getLog(chatId, page)
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
      r       <- user.advance(command)
    } yield r

  private def getLog(chatId: Long, page: Int): F[UserStatsResult] =
    for {
      _   <- Logger[F].debug(s"[$chatId] Getting log statistic")
      log <- statistic.getLog(chatId, page)
    } yield log

  private def getStats(chatId: Long, statsData: StatsData): F[UserStatsResult] =
    statsData match {
      case StatsData.StatsLogData =>
        for {
          _   <- Logger[F].debug(s"[$chatId] Getting log statistic")
          log <- statistic.getLog(chatId, 0)
        } yield log
      case StatsData.StatsCountPerDayData =>
        for {
          _         <- Logger[F].debug(s"[$chatId] Getting completed last day statistic")
          completed <- statistic.getCompletedLastDay(chatId)
        } yield completed
      case StatsData.StatsCountPerWeekData =>
        for {
          _         <- Logger[F].debug(s"[$chatId] Getting completed last week statistic")
          completed <- statistic.getCompletedLastWeek(chatId)
        } yield completed
      case StatsData.StatsCountPerMonthData =>
        for {
          _         <- Logger[F].debug(s"[$chatId] Getting completed last month statistic")
          completed <- statistic.getCompletedLastMonth(chatId)
        } yield completed
    }
}

case object TomodoroInterpreter {

  def apply[I[_]: Monad, F[_]: Logger: Monad](
      users: Users[F],
      statistic: Statistic[F],
      telegram: Telegram[F]
  ): I[Tomodoro[F]] =
    for {
      _              <- Monad[I].unit
      t: Tomodoro[F] = new TomodoroInterpreter[F](users, statistic, telegram)
    } yield t

  def apply[F[_]: Sync](
      users: Users[F],
      statistic: Statistic[F],
      telegram: Telegram[F]
  ): F[Tomodoro[F]] = {
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      t                            <- apply[F, F](users, statistic, telegram)
    } yield t
  }

  private def now: Long = OffsetDateTime.now().toEpochSecond

  private object commandOrQuery {
    def unapply(update: TUpdate): Option[(Long, String)] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some(text))), None) => (chatId, text).some
      case _                                                              => none
    }
  }

  private object settingsValue {
    def unapply(update: TUpdate): Option[(Long, Int)] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some(value))), None) if value.forall(Character.isDigit) =>
        (chatId, value.toInt).some
      case _ => none
    }
  }

  private object settingsCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, String, UserCommand)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data)))) =>
        SettingsData.withNameOption(data).map(d => (chatId, callbackId, d.cmd(now)))
      case _ => none
    }
  }

  private object statsCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, String, StatsData)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data)))) =>
        StatsData.withNameOption(data).map(d => (chatId, callbackId, d))
      case _ => none
    }
  }

  private object statsLogCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, String, Int)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data))))
          if data.startsWith(StatsData.StatsLogData.entryName) =>
        Try(data.replaceFirst(s"${StatsData.StatsLogData.entryName}:", "").toInt).toOption
          .map(p => (chatId, callbackId, p))
      case _ => none
    }
  }
}
