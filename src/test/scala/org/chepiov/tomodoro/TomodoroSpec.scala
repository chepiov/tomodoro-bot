package org.chepiov.tomodoro

import cats._
import org.chepiov.tomodoro.algebras.Logger
import org.chepiov.tomodoro.typeclasses.FromFuture
import org.slf4j.LoggerFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait TomodoroSpec {

  implicit val idLogger: Logger[Id] = new Logger[Id] {

    private val logger = LoggerFactory.getLogger("test")

    override def info(message: => String): Id[Unit] =
      logger.info(message)

    override def debug(message: => String): Id[Unit] =
      logger.debug(message)

    override def warn(message: => String): Id[Unit] =
      logger.warn(message)

    override def error(e: Throwable)(message: => String): Id[Unit] =
      logger.error(message, e)
  }

  implicit val idFromFuture: FromFuture[Id] = new FromFuture[Id] {
    override def fromFuture[A](a: Id[Future[A]]): Id[A] =
      Await.result(a, 5.second)
  }
}
