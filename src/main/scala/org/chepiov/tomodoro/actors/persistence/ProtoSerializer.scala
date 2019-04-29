package org.chepiov.tomodoro.actors.persistence

import java.io.NotSerializableException

import akka.serialization.SerializerWithStringManifest
import org.chepiov.tomodoro.actors.persistence.userMessage.{PMessageConfirmedEvent, PMessageSentEvent}
import org.chepiov.tomodoro.actors.persistence.userPersistence.PStateChangedEvent

class ProtoSerializer extends SerializerWithStringManifest {

  final val StateChangedEventManifest     = classOf[PStateChangedEvent].getName
  final val MessageSendEventManifest      = classOf[PMessageSentEvent].getName
  final val MessageConfirmedEventManifest = classOf[PMessageConfirmedEvent].getName

  override def identifier: Int = 42

  override def manifest(o: AnyRef): String = o.getClass.getName

  override def fromBinary(bytes: Array[Byte], manifest: String): AnyRef =
    manifest match {
      case StateChangedEventManifest     => PStateChangedEvent.parseFrom(bytes)
      case MessageSendEventManifest      => PMessageSentEvent.parseFrom(bytes)
      case MessageConfirmedEventManifest => PMessageConfirmedEvent.parseFrom(bytes)
      case _                             => throw new NotSerializableException(s"Unable to handle manifest: $manifest")
    }

  override def toBinary(o: AnyRef): Array[Byte] =
    o match {
      case e: PStateChangedEvent     => println("SERIALIZER"); e.toByteArray
      case e: PMessageSentEvent      => e.toByteArray
      case e: PMessageConfirmedEvent => e.toByteArray
      case _                         => throw new NotSerializableException(s"Unable to handle object: $o")
    }
}
