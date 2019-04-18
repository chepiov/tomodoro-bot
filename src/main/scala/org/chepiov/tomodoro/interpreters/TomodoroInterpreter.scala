package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import io.chrisdavenport.log4cats.Logger
import org.chepiov.tomodoro.algebras.Telegram._
import org.chepiov.tomodoro.algebras.User.{apply => _, _}
import org.chepiov.tomodoro.algebras.{Telegram, Tomodoro, Users}
import org.chepiov.tomodoro.programs.UserStateMachine.{
  SettingAmountData,
  SettingDurationData,
  SettingLongBreakData,
  SettingShortBreakData
}

class TomodoroInterpreter[F[_]: Logger: Monad](
    users: Users[F],
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
      case TUpdate(_, Some(TMessage(_, TChat(chatId), Some(text))), None) =>
        text match {
          case "/help" =>
            for {
              _    <- Logger[F].debug("Received help request")
              user <- users.getOrCreateUser(chatId)
              r    <- user.info(GetHelp)
            } yield r
          case "/start" =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received start command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(Continue(now))
            } yield r
          case "/continue" =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received continue command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(Continue(now))
            } yield r
          case "/pause" =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received suspend command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(Suspend(now))
            } yield r
          case "/reset" =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received reset command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(Reset(now))
            } yield r
          case "/skip" =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received skip command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(Skip(now))
            } yield r
          case "/settings" =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received settings command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(SetSettings(now))
            } yield r
          case settingsValue if settingsValue.forall(Character.isDigit) =>
            for {
              _    <- Logger[F].debug(s"[$chatId] Received settings update command")
              user <- users.getOrCreateUser(chatId)
              r    <- user.advance(SetSettingsValue(now, settingsValue.toInt))
            } yield r
          case m =>
            for {
              _    <- Logger[F].warn(s"[$chatId] Received unknown message: $m")
              user <- users.getOrCreateUser(chatId)
              r    <- user.info(GetHelp)
            } yield r
        }
      case TUpdate(_, None, Some(TCallbackQuery(callbackId, _, Some(TMessage(_, TChat(chatId), _)), Some(data)))) =>
        for {
          _ <- Logger[F].debug(s"Received callback query with data: $data")
          _ <- telegram.answerCallbackQuery(TCallbackAnswer(callbackId))
          cmd = data match {
            case SettingDurationData =>
              AwaitChangingDuration(now).some
            case SettingLongBreakData =>
              AwaitChangingLongBreak(now).some
            case SettingShortBreakData =>
              AwaitChangingShortBreak(now).some
            case SettingAmountData =>
              AwaitChangingAmount(now).some
            case _ => none[UserCommand]
          }
          result <- if (cmd.isDefined)
                     for {
                       user <- users.getOrCreateUser(chatId)
                       r    <- user.advance(cmd.get)
                     } yield r
                   else Monad[F].unit
        } yield result
      case _ =>
        for {
          r <- Logger[F].warn(s"Unknown update: $update")
        } yield r
    }
  }

  override def setWebHook(updateUrl: String): F[Unit] = Monad[F].unit

  override def deleteWebHook(): F[Unit] = Monad[F].unit
}

case object TomodoroInterpreter {
  def apply[I[_]: Monad, F[_]: Logger: Monad](
      users: Users[F],
      telegram: Telegram[F]
  ): I[Tomodoro[F]] =
    for {
      _ <- Monad[I].unit
      t = new TomodoroInterpreter[F](users, telegram)
    } yield t

  def apply[F[_]: Logger: Monad](
      users: Users[F],
      telegram: Telegram[F]
  ): F[Tomodoro[F]] = apply[F, F](users, telegram)

  private def now: Long = OffsetDateTime.now().toEpochSecond
}
