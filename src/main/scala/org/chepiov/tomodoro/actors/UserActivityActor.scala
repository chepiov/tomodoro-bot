package org.chepiov.tomodoro.actors

import akka.actor.{ActorLogging, Props}
import akka.contrib.persistence.mongodb.{MongoReadJournal, ScalaDslMongoReadJournal}
import akka.persistence.query._
import akka.persistence.{PersistentActor, RecoveryCompleted, SnapshotOffer}
import akka.stream.scaladsl.Sink
import akka.stream.{ActorMaterializer, Materializer}
import cats.effect.Effect
import cats.effect.syntax.effect._
import org.chepiov.tomodoro.actors.persistence.userPersistence.PStateChangedEvent
import org.chepiov.tomodoro.programs.UserActivity.StateChangedEvent
import org.chepiov.tomodoro.actors.persistence.ProtoEventAdapter._
import org.chepiov.tomodoro.algebras.Iso.syntax._

import scala.util.{Failure, Success, Try}

/**
  * Represents user activity recorder.
  *
  * @param chatId   user chat id
  * @param consumer consumer of user activity stream
  */
class UserActivityActor[F[_]: Effect](chatId: Long, consumer: StateChangedEvent => F[Try[Unit]])
    extends PersistentActor with ActorLogging {

  import UserActivityActor._

  override def persistenceId = s"user-activity-$chatId"

  private val statJournal: ScalaDslMongoReadJournal =
    PersistenceQuery(context.system).readJournalFor[ScalaDslMongoReadJournal](MongoReadJournal.Identifier)

  implicit val mat: Materializer = ActorMaterializer()

  override def receiveRecover: Receive = {
    case o: Long                   => context.become(working(o))
    case SnapshotOffer(_, o: Long) => context.become(working(o))
    case RecoveryCompleted =>
      log.debug(s"[$chatId] Recovering completed")
      self ! InitCmd
  }

  override def receiveCommand: Receive = {
    working(0L)
  }

  private def working(sequenceNr: Long): Receive = {
    case InitCmd =>
      log.debug(s"[$chatId] Consuming will be started from $sequenceNr")
      statJournal
        .eventsByPersistenceId(s"user-$chatId", sequenceNr, Long.MaxValue)
        .map {
          case env @ EventEnvelope(_, _, _, event: PStateChangedEvent) =>
            env.copy(event = event.unwrap[StateChangedEvent])
          case env => env
        }
        .runWith(Sink.actorRefWithAck(self, StreamInitialized(sequenceNr), Ack, StreamCompleted, StreamFailure))
      ()
    case StreamInitialized(nr) =>
      log.debug(s"[$chatId] Consumer initialized from $nr")
      sender() ! Ack
    case EventEnvelope(_, _, nextNr, evt: StateChangedEvent) =>
      consumer(evt).toIO.unsafeRunSync() match {
        case Success(_) =>
          persist(nextNr + 1) { nr =>
            log.debug(s"[$chatId] sequenceNr $nr persisted")
            context.become(working(nr))
          }
        case Failure(e) =>
          log.error(e, s"[$chatId] Can't consume user event")
      }
      sender() ! Ack
    case _: EventEnvelope =>
      sender() ! Ack
    case StreamCompleted =>
      log.debug(s"[$chatId] Consumer completed")
      sender() ! Ack
    case StreamFailure(e) =>
      log.error(e, s"[$chatId] Error during consuming")
    case m =>
      log.warning(s"[$chatId] Stat actor should not receive any commands. Sender: ${sender()}, message: $m")
  }
}

case object UserActivityActor {

  def props[F[_]: Effect](chatId: Long, consumer: StateChangedEvent => F[Try[Unit]]): Props =
    Props(new UserActivityActor[F](chatId, consumer))

  private val InitCmd = "INIT"

  private case object Ack

  private case class StreamInitialized(sequenceNr: Long)

  private case object StreamCompleted

  private case class StreamFailure(ex: Throwable)

}
