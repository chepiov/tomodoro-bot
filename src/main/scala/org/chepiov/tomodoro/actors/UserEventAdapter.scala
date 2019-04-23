package org.chepiov.tomodoro.actors

import akka.persistence.journal.{Tagged, WriteEventAdapter}
import org.chepiov.tomodoro.actors.UserActor.MessageSentEvent
import org.chepiov.tomodoro.actors.UserActor.MessageConfirmedEvent

class UserEventAdapter extends WriteEventAdapter {

  override def toJournal(event: Any): Any =
    event match {
      case evt: MessageSentEvent =>
        Tagged(evt, Set("user-message"))
      case evt: MessageConfirmedEvent =>
        Tagged(evt, Set("user-message"))
      case _ => event
    }

  override def manifest(event: Any): String = ""
}
