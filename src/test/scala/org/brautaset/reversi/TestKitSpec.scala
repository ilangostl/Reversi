package org.brautaset.reversi

import akka.actor.ActorSystem
import akka.testkit.{ ImplicitSender, TestKit }
import org.scalatest.{ WordSpec, BeforeAndAfterAll }
import org.scalatest.matchers.MustMatchers

abstract class TestKitSpec(name: String)
  extends TestKit(ActorSystem(name))
  with WordSpec
  with MustMatchers
  with BeforeAndAfterAll
  with ImplicitSender {

  override def afterAll() {
    system.shutdown()
  }
}