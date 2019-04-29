package org.chepiov.tomodoro.actors.persistence

import java.io.NotSerializableException

import akka.serialization.SerializerWithStringManifest
import org.chepiov.tomodoro.actors.persistence.userMessage.{PMessageConfirmedEvent, PMessageSentEvent}
import org.chepiov.tomodoro.actors.persistence.userPersistence.PStateChangedEvent

class ProtoSerializer extends SerializerWithStringManifest {

  final val stateChangedEventManifest     = classOf[PStateChangedEvent].getName
  final val messageSendEventManifest      = classOf[PMessageSentEvent].getName
  final val messageConfirmedEventManifest = classOf[PMessageConfirmedEvent].getName

  override def identifier: Int = 42

  override def manifest(o: AnyRef): String = o.getClass.getName

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef =
    manifest match {
      case `stateChangedEventManifest`     => PStateChangedEvent.parseFrom(bytes)
      case `messageSendEventManifest`      => PMessageSentEvent.parseFrom(bytes)
      case `messageConfirmedEventManifest` => PMessageConfirmedEvent.parseFrom(bytes)
      case _                               => throw new NotSerializableException(s"Unable to handle manifest: $manifest")
    }

  override def toBinary(o: AnyRef): Array[Byte] =
    o match {
      case e: PStateChangedEvent     => e.toByteArray
      case e: PMessageSentEvent      => e.toByteArray
      case e: PMessageConfirmedEvent => e.toByteArray
      case _                         => throw new NotSerializableException(s"Unable to handle object: $o")
    }
}
