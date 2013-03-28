package org.brautaset.reversi

import akka.testkit.TestProbe
import akka.actor.Props

class MatchSpec extends TestKitSpec("MatchSpec") {

  import Match._

  trait Case {
    val p1 = TestProbe()
    val p2 = TestProbe()
    val mat = system.actorOf(Props(new Match(p1.ref, p2.ref)))
  }

  "A Match in initial state" should {

    "respond to Go" in new Case {
      mat ! Go
      p1.expectMsg(Move(Board()))
    }

    "respond to Check" in new Case {
      mat ! Check
      expectMsg(Prestart(Board()))
    }

    "not accept Claim" in new Case {
      mat ! Claim(Location(0, 0))
      expectNoMsg()
    }

  }

  "An Ongoing match" should {

    "respond to Go" in new Case {
      mat ! Go // move out of starting state

      var board = Board()
      p1.expectMsg(Move(board))

      val m1 = Location(4, 5)
      p1.reply(Claim(m1))

      board = board.successor(m1)
      mat ! Check
      expectMsg(Ongoing(board, p2.ref))

      val m2 = Location(5, 3)
      val m3 = Location(4, 2)
      val m4 = Location(5, 5)
      val m5 = Location(6, 4)
      val m6 = Location(3, 5)
      val m7 = Location(4, 6)
      val m8 = Location(5, 5)
      val m9 = Location(2, 4)


    }
  }

}
