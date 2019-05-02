package org.chepiov.tomodoro.interpreters

import cats.Monad
import cats.effect.{Sync, Timer}
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User.{apply => _, _}
import org.chepiov.tomodoro.algebras.{Statistic, Telegram, Tomodoro, Users}
import org.chepiov.tomodoro.programs.UserMessageData.StatsData._
import org.chepiov.tomodoro.programs.UserMessageData._
import org.chepiov.tomodoro.programs.UserMessages._

import scala.concurrent.duration.SECONDS
import scala.util.Try

class TomodoroInterpreter[F[_]: Logger: Monad: Timer](
    users: Users[F],
    statistic: Statistic[F],
    telegram: Telegram[F]
) extends Tomodoro[F] {
  import TomodoroInterpreter._

  override def getInfo: F[TUser] = debug("Received info request") *> telegram.getMe

  override def handleUpdate(update: TUpdate): F[Unit] = update match {
    case cmdOrQuery(chatId, text) if stateSynonyms.contains(text)    => query(chatId, GetState)
    case cmdOrQuery(chatId, text) if statsSynonyms.contains(text)    => query(chatId, GetStats)
    case cmdOrQuery(chatId, text) if helpSynonyms.contains(text)     => query(chatId, GetHelp)
    case cmdOrQuery(chatId, text) if continueSynonyms.contains(text) => advance(chatId, Continue)
    case cmdOrQuery(chatId, text) if pauseSynonyms.contains(text)    => advance(chatId, Suspend)
    case cmdOrQuery(chatId, text) if resetSynonyms.contains(text)    => advance(chatId, Reset)
    case cmdOrQuery(chatId, text) if skipSynonyms.contains(text)     => advance(chatId, Skip)
    case cmdOrQuery(chatId, text) if settingsSynonyms.contains(text) => advance(chatId, SetSettings)
    case settingsValue((chatId, value))                              => advance(chatId, SetSettingsValue.apply(_, value))
    case settingsCallbackQuery(chatId, callbackId, cmd)              => settingsCallback(chatId, callbackId, cmd)
    case statsCallbackQuery(chatId, callbackId, statsType)           => statsCallback(chatId, callbackId, statsType)
    case logCallbackQuery(chatId, messageId, callbackId, page)       => logCallback(chatId, messageId, callbackId, page)
    case TUpdate(_, Some(TMessage(_, TChat(chatId), text)), _)       => unknownMessage(chatId, text)
    case _                                                           => Logger[F].warn(s"Unknown update: $update")
  }

  override def setWebHook(updateUrl: String): F[Unit] = Monad[F].unit

  override def deleteWebHook(): F[Unit] = Monad[F].unit

  private def query(chatId: Long, query: UserInfoQuery): F[Unit] =
    for {
      _    <- debug(s"[$chatId] Received $query query")
      user <- users.getOrCreateUser(chatId)
      r    <- user.info(query)
    } yield r

  private def advance(chatId: Long, cmd: Long => UserCommand): F[Unit] =
    for {
      user    <- users.getOrCreateUser(chatId)
      now     <- Timer[F].clock.realTime(SECONDS)
      command = cmd(now)
      _       <- debug(s"[$chatId] Received $command command")
      r       <- user.advance(command)
    } yield r

  private def settingsCallback(chatId: Long, callbackId: String, cmd: Long => UserCommand): F[Unit] =
    advance(chatId, cmd) *> telegram.answerCallbackQuery(TCallbackAnswer(callbackId))

  private def statsCallback(chatId: Long, callbackId: String, statsType: StatsData): F[Unit] =
    for {
      _ <- debug(s"[$chatId] Received stats callback query, type: ${statsType.entryName}")
      _ <- sendStats(chatId, statsType)
      r <- telegram.answerCallbackQuery(TCallbackAnswer(callbackId))
    } yield r

  private def logCallback(chatId: Long, messageId: Long, callbackId: String, page: Int): F[Unit] =
    for {
      _ <- debug(s"[$chatId] Received log page callback query, page: $page")
      _ <- sendActivity(chatId, page, messageId.some)
      r <- telegram.answerCallbackQuery(TCallbackAnswer(callbackId))
    } yield r

  private def unknownMessage(chatId: Long, text: Option[String]): F[Unit] =
    Logger[F].warn(s"[$chatId] Unknown message: $text") *> telegram.sendMessage(unknownMsg(chatId))

  private def sendActivity(chatId: Long, page: Int, messageId: Option[Long] = None): F[Unit] =
    debug(s"[$chatId] Sending activity log") *> statistic.sendActivity(chatId, page, messageId)

  private def sendStats(chatId: Long, statsData: StatsData): F[Unit] =
    statsData match {
      case StatsLogData =>
        sendActivity(chatId, 0)
      case StatsCountPerDayData =>
        debug(s"[$chatId] Sending completed last day statistic") *> statistic.sendCompletedLastDay(chatId)
      case StatsCountPerWeekData =>
        debug(s"[$chatId] Sending completed last week statistic") *> statistic.sendCompletedLastWeek(chatId)
      case StatsCountPerMonthData =>
        debug(s"[$chatId] Sending completed last month statistic") *> statistic.sendCompletedLastMonth(chatId)
    }

  private def debug(msg: => String): F[Unit] = Logger[F].debug(msg)
}

case object TomodoroInterpreter {

  def apply[I[_]: Monad, F[_]: Logger: Monad: Timer](
      users: Users[F],
      statistic: Statistic[F],
      telegram: Telegram[F]
  ): I[Tomodoro[F]] =
    for {
      _              <- Monad[I].unit
      t: Tomodoro[F] = new TomodoroInterpreter[F](users, statistic, telegram)
    } yield t

  def apply[F[_]: Sync: Timer](
      users: Users[F],
      statistic: Statistic[F],
      telegram: Telegram[F]
  ): F[Tomodoro[F]] = {
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.create
      t                            <- apply[F, F](users, statistic, telegram)
    } yield t
  }

  private object cmdOrQuery {
    def unapply(update: TUpdate): Option[(Long, String)] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some(text))), None) => (chatId, text).some
      case _                                                              => none
    }
  }

  private object settingsValue {
    def unapply(update: TUpdate): Option[(Long, Int)] = update match {
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some(value))), None)
          if value.forall(Character.isDigit) && value.toInt > 0 =>
        (chatId, value.toInt).some
      case _ => none
    }
  }

  private object settingsCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, String, Long => UserCommand)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data)))) =>
        SettingsData.withNameOption(data).map(d => (chatId, callbackId, d.cmd))
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

  private object logCallbackQuery {
    def unapply(update: TUpdate): Option[(Long, Long, String, Int)] = update match {
      case TUpdate(_, None, Some(TCallbackQuery(cbId, _, Some(TMessage(mId, TChat(chatId), _)), Some(data))))
          if data.startsWith(StatsLogData.entryName) =>
        Try(data.replaceFirst(s"${StatsLogData.entryName}:", "").toInt).toOption
          .map(p => (chatId, mId, cbId, p))
      case _ => none
    }
  }
}
