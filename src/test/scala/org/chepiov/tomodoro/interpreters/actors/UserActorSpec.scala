package org.chepiov.tomodoro.interpreters.actors

import akka.actor.ActorSystem
import akka.testkit.TestKit
import com.typesafe.config.ConfigFactory

class UserActorSpec extends TestKit(ActorSystem("test-system", ConfigFactory.load("application-persistence-test"))) {}
