package org.chepiov.tomodoro.actors

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import akka.testkit.TestKit
import org.scalatest.BeforeAndAfterAll

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
trait PersistenceSpec { self: TestKit with BeforeAndAfterAll =>
  override def beforeAll(): Unit = {
    List(
      "akka.persistence.journal.leveldb.dir",
      "akka.persistence.snapshot-store.local.dir"
    ).filter { s =>
      val path = Paths.get(system.settings.config.getString(s))
      Files.exists(path) && Files.isDirectory(path)
    }.foreach { s =>
      Files.walkFileTree(
        Paths.get(system.settings.config.getString(s)),
        new SimpleFileVisitor[Path] {
          override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
            Files.deleteIfExists(file)
            FileVisitResult.CONTINUE
          }
          override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
            Files.deleteIfExists(dir)
            FileVisitResult.CONTINUE
          }
        }
      )
    }
  }
}

object PersistenceSpec {
  def config(dir: String): String =
    s"""
       |akka.persistence.journal.plugin = "akka.persistence.journal.leveldb"
       |akka.persistence.snapshot-store.plugin = "akka.persistence.snapshot-store.local"
       |akka.persistence.journal.leveldb.dir = "target/$dir/journal"
       |akka.persistence.snapshot-store.local.dir = "target/$dir/snapshots"
       |akka.persistence.journal.leveldb.native = false
  """.stripMargin
}
