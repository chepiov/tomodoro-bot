package org.chepiov.tomodoro.actors.persistence

import java.io.NotSerializableException

import akka.persistence.journal.{EventAdapter, EventSeq}
import cats.syntax.option._
import org.chepiov.tomodoro.actors.UserActor.{MessageConfirmedEvent, MessageSentEvent}
import org.chepiov.tomodoro.actors.persistence.userMessage.PSendMessage.ReplyMarkup
import org.chepiov.tomodoro.actors.persistence.userMessage._
import org.chepiov.tomodoro.actors.persistence.userPersistence.PCommand.Discriminator._
import org.chepiov.tomodoro.actors.persistence.userPersistence.PSettingsUpdate.Discriminator._
import org.chepiov.tomodoro.actors.persistence.userPersistence.PStatus.Discriminator._
import org.chepiov.tomodoro.actors.persistence.userPersistence._
import org.chepiov.tomodoro.algebras.Iso
import org.chepiov.tomodoro.algebras.Iso.syntax._
import org.chepiov.tomodoro.algebras.Telegram.{apply => _, _}
import org.chepiov.tomodoro.algebras.User._
import org.chepiov.tomodoro.programs.UserActivity.StateChangedEvent

class ProtoEventAdapter extends EventAdapter {
  import ProtoEventAdapter._

  override def manifest(event: Any): String = ""

  override def toJournal(event: Any): Any = event match {
    case m: StateChangedEvent     => m.wrap[PStateChangedEvent]
    case m: MessageSentEvent      => m.wrap[PMessageSentEvent]
    case m: MessageConfirmedEvent => m.wrap[PMessageConfirmedEvent]
    case e                        => e
  }

  override def fromJournal(event: Any, manifest: String): EventSeq =
    event match {
      case p: PStateChangedEvent     => EventSeq(p.unwrap[StateChangedEvent])
      case p: PMessageSentEvent      => EventSeq(p.unwrap[MessageSentEvent])
      case p: PMessageConfirmedEvent => EventSeq(p.unwrap[MessageConfirmedEvent])
      case e                         => EventSeq(e)
    }
}

case object ProtoEventAdapter {

  implicit val commandIso: Iso[Command, PCommand] = new Iso[Command, PCommand] {

    override def wrap(a: Command): PCommand =
      a match {
        case _: Continue                => PCommand(a.time, CONTINUE)
        case _: Finish                  => PCommand(a.time, FINISH)
        case _: Suspend                 => PCommand(a.time, SUSPEND)
        case _: Reset                   => PCommand(a.time, RESET)
        case _: Skip                    => PCommand(a.time, SKIP)
        case _: SetSettings             => PCommand(a.time, SET_SETTINGS)
        case _: AwaitChangingDuration   => PCommand(a.time, AWAIT_CHANGING_DURATION)
        case _: AwaitChangingLongBreak  => PCommand(a.time, AWAIT_CHANGING_LONG_BREAK)
        case _: AwaitChangingShortBreak => PCommand(a.time, AWAIT_CHANGING_SHORT_BREAK)
        case _: AwaitChangingAmount     => PCommand(a.time, AWAIT_CHANGING_AMOUNT)
        case s: SetSettingsValue        => PCommand(a.time, SET_SETTINGS_VALUE, s.value.some)
      }

    override def unwrap(b: PCommand): Command = b.discriminator match {
      case CONTINUE                                       => Continue(b.time)
      case FINISH                                         => Finish(b.time)
      case SUSPEND                                        => Suspend(b.time)
      case RESET                                          => Reset(b.time)
      case SKIP                                           => Skip(b.time)
      case SET_SETTINGS                                   => SetSettings(b.time)
      case AWAIT_CHANGING_DURATION                        => AwaitChangingDuration(b.time)
      case AWAIT_CHANGING_LONG_BREAK                      => AwaitChangingLongBreak(b.time)
      case AWAIT_CHANGING_SHORT_BREAK                     => AwaitChangingShortBreak(b.time)
      case AWAIT_CHANGING_AMOUNT                          => AwaitChangingAmount(b.time)
      case SET_SETTINGS_VALUE if b.settingValue.isDefined => SetSettingsValue(b.time, b.settingValue.get)
      case _                                              => throw new NotSerializableException(s"Unable to handle object: $b")
    }
  }

  implicit val userSettingsIso: Iso[UserSettings, PUserSettings] =
    new Iso[UserSettings, PUserSettings] {

      override def wrap(a: UserSettings): PUserSettings =
        PUserSettings(a.duration, a.shortBreak, a.longBreak, a.amount)

      override def unwrap(b: PUserSettings): UserSettings =
        UserSettings(b.duration, b.shortBreak, b.longBreak, b.amount)
    }

  implicit val userStatusIso: Iso[Status, PStatus] =
    new Iso[Status, PStatus] {

      override def wrap(a: Status): PStatus =
        a match {
          case WaitingWork(remaining, startTime) =>
            PStatus(remaining, startTime, WAITING_WORK)
          case WaitingBreak(remaining, startTime) =>
            PStatus(remaining, startTime, WAITING_BREAK)
          case Working(remaining, startTime, endTime) =>
            PStatus(remaining, startTime, WORKING, endTime = endTime.some)
          case Breaking(remaining, startTime, endTime) =>
            PStatus(remaining, startTime, BREAKING, endTime = endTime.some)
          case WorkSuspended(remaining, startTime, suspend) =>
            PStatus(remaining, startTime, WORK_SUSPENDED, suspend = suspend.some)
          case BreakSuspended(remaining, startTime, suspend) =>
            PStatus(remaining, startTime, BREAK_SUSPENDED, suspend = suspend.some)
        }

      override def unwrap(b: PStatus): Status =
        b.discriminator match {
          case WAITING_WORK                           => WaitingWork(b.remaining, b.startTime)
          case WAITING_BREAK                          => WaitingBreak(b.remaining, b.startTime)
          case WORKING if b.endTime.isDefined         => Working(b.remaining, b.startTime, b.endTime.get)
          case BREAKING if b.endTime.isDefined        => Breaking(b.remaining, b.startTime, b.endTime.get)
          case WORK_SUSPENDED if b.suspend.isDefined  => WorkSuspended(b.remaining, b.startTime, b.suspend.get)
          case BREAK_SUSPENDED if b.suspend.isDefined => BreakSuspended(b.remaining, b.startTime, b.suspend.get)
          case _                                      => throw new NotSerializableException(s"Unable to handle object: $b")
        }
    }

  implicit val userSettingsUpdateIso: Iso[SettingsUpdate, PSettingsUpdate] =
    new Iso[SettingsUpdate, PSettingsUpdate] {

      override def wrap(a: SettingsUpdate): PSettingsUpdate =
        a match {
          case DurationUpdate(startedAt)   => PSettingsUpdate(startedAt.some, DURATION)
          case ShortBreakUpdate(startedAt) => PSettingsUpdate(startedAt.some, SHORT_BREAK)
          case LongBreakUpdate(startedAt)  => PSettingsUpdate(startedAt.some, LONG_BREAK)
          case AmountUpdate(startedAt)     => PSettingsUpdate(startedAt.some, AMOUNT)
          case NotUpdate                   => PSettingsUpdate(None, NOT)
        }

      override def unwrap(b: PSettingsUpdate): SettingsUpdate =
        b.discriminator match {
          case DURATION if b.startedAt.isDefined    => DurationUpdate(b.startedAt.get)
          case SHORT_BREAK if b.startedAt.isDefined => ShortBreakUpdate(b.startedAt.get)
          case LONG_BREAK if b.startedAt.isDefined  => LongBreakUpdate(b.startedAt.get)
          case AMOUNT if b.startedAt.isDefined      => AmountUpdate(b.startedAt.get)
          case NOT                                  => NotUpdate
          case _                                    => throw new NotSerializableException(s"Unable to handle object: $b")
        }
    }

  implicit val userStateIso: Iso[UserState, PUserState] =
    new Iso[UserState, PUserState] {

      override def wrap(a: UserState): PUserState =
        PUserState(a.settings.wrap, a.status.wrap, a.settingsUpdate.wrap)

      override def unwrap(b: PUserState): UserState =
        UserState(b.settings.unwrap, b.status.unwrap, b.settingsUpdate.unwrap)
    }

  implicit val stateChangeEventIso: Iso[StateChangedEvent, PStateChangedEvent] =
    new Iso[StateChangedEvent, PStateChangedEvent] {

      def wrap(a: StateChangedEvent): PStateChangedEvent =
        PStateChangedEvent(a.chatId, a.state.wrap, a.cmd.wrap)

      def unwrap(b: PStateChangedEvent): StateChangedEvent =
        StateChangedEvent(b.chatId, b.state.unwrap, b.cmd.unwrap)
    }

  implicit val sendMessageIso: Iso[TSendMessage, PSendMessage] =
    new Iso[TSendMessage, PSendMessage] {

      override def wrap(a: TSendMessage): PSendMessage = {
        val markup = a.replyMarkup match {
          case Some(TReplyKeyboardMarkup(keyboard)) =>
            val buttons = keyboard.map(k => PReplyButtons(k.map(_.text)))
            ReplyMarkup.ReplyKeyboard(PReplyKeyboard(buttons))
          case Some(TInlineKeyboardMarkup(keyboard)) =>
            val buttons = keyboard.map(k => PInlineButtons(k.map(b => PInlineButton(b.text, b.callbackData))))
            ReplyMarkup.InlineKeyboard(PInlineKeyboard(buttons))
          case None => ReplyMarkup.Empty
        }
        PSendMessage(a.chatId, a.text, a.parseMode, markup)
      }

      override def unwrap(b: PSendMessage): TSendMessage = {
        val markup = b.replyMarkup match {
          case ReplyMarkup.ReplyKeyboard(keyboard) =>
            val buttons = keyboard.rows
              .map(r => r.buttons.map(text => TKeyboardButton(text)).toList)
              .toList
            TReplyKeyboardMarkup(buttons).some
          case ReplyMarkup.InlineKeyboard(keyboard) =>
            val buttons = keyboard.rows
              .map(r => r.buttons.map(button => TInlineKeyboardButton(button.text, button.callbackData)).toList)
              .toList
            TInlineKeyboardMarkup(buttons).some
          case ReplyMarkup.Empty =>
            none[TReplyMarkup]
        }
        TSendMessage(b.chatId, b.text, markup, b.parseMode)
      }
    }

  implicit val messageSentEventIso: Iso[MessageSentEvent, PMessageSentEvent] =
    new Iso[MessageSentEvent, PMessageSentEvent] {
      override def wrap(a: MessageSentEvent): PMessageSentEvent =
        PMessageSentEvent(a.message.wrap)

      override def unwrap(b: PMessageSentEvent): MessageSentEvent =
        MessageSentEvent(b.message.unwrap)
    }

  implicit val messageConfirmedEventIso: Iso[MessageConfirmedEvent, PMessageConfirmedEvent] =
    new Iso[MessageConfirmedEvent, PMessageConfirmedEvent] {
      override def wrap(a: MessageConfirmedEvent): PMessageConfirmedEvent =
        PMessageConfirmedEvent(a.deliveryId)

      override def unwrap(b: PMessageConfirmedEvent): MessageConfirmedEvent =
        MessageConfirmedEvent(b.deliveryId)
    }
}
