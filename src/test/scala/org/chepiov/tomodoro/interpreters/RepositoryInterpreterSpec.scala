package org.chepiov.tomodoro.interpreters

import java.time.OffsetDateTime

import cats.effect.{ContextShift, IO}
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import doobie.scalatest.IOChecker
import doobie.util.transactor.Transactor
import org.chepiov.tomodoro.algebras.Repository.{ActivityDescriptor, ActivityLog}
import org.flywaydb.core.Flyway
import org.scalatest.{Matchers, WordSpecLike}

import scala.concurrent.ExecutionContext

@SuppressWarnings(Array("org.wartremover.warts.Null", "org.wartremover.warts.NonUnitStatements"))
class RepositorySQLSpec extends WordSpecLike with Matchers with IOChecker with ForAllTestContainer {
  import RepositorySQL._

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  override val container = PostgreSQLContainer()

  override def transactor: doobie.Transactor[IO] =
    Transactor.fromDriverManager(
      container.driverClassName,
      container.jdbcUrl,
      container.username,
      container.password
    )

  override def afterStart(): Unit = {
    Class.forName(container.driverClassName)

    val fw = Flyway
      .configure()
      .dataSource(container.jdbcUrl, container.username, container.password)
      .load()
    fw.migrate()
    ()
  }

  "Repository" should {
    "create correct query for finding logs" in {
      check(findLogs(1L, 0L, 10L))
    }
  }

  it should {
    "create correct query for counting completed" in {
      check(countCompleted(1L, OffsetDateTime.MIN, OffsetDateTime.now()))
    }
  }

  it should {
    "create correct query for adding log" in {
      check(addLog(ActivityLog(1L, OffsetDateTime.now(), ActivityDescriptor.CycleReset, "test")))
    }
  }
}
