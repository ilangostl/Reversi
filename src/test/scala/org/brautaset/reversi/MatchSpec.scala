package org.brautaset.reversi

import akka.testkit.TestProbe
import akka.actor.Props

class MatchSpec extends TestKitSpec("MatchSpec") {

  import Match._

  trait Case {
    val p1 = TestProbe()
    val p2 = TestProbe()
    val driver = TestProbe()
    val board = Board()
    val mat = system.actorOf(Props(new Match(p1.ref, p2.ref, driver.ref)))
  }

  "Go" should {

    "ask player 1 to move" in new Case {
      mat ! Go
      p1.expectMsg(Move(board))
    }

  }

  "Check" should {

    "tell us the current board & player turn" in new Case {
      mat ! Check
      expectMsg(Ongoing(board, p1.ref))
    }

  }

  "Claim" should {

    "do nothing unless it is from current player" in new Case {
      mat ! Claim(Location(4, 5))
      driver.expectNoMsg()
    }

    "advance the game to its successor state" in new Case {
      mat.tell(Claim(Location(4, 5)), p1.ref)
      driver.expectMsg(Ongoing(board.successor(Location(4, 5)), p2.ref))
    }

    "eventually cause game over" in new Case {

      val moves = List(
        (p1.ref, Location(4, 5)),
        (p2.ref, Location(5, 3)),
        (p1.ref, Location(4, 2)),
        (p2.ref, Location(5, 5)),
        (p1.ref, Location(6, 4)),
        (p2.ref, Location(3, 5)),
        (p1.ref, Location(4, 6)),
        (p2.ref, Location(5, 5)),
        (p1.ref, Location(2, 4)))

      moves.map { x =>
        mat.tell(x._2, x._1)
        expectMsgType[Ongoing]
      }

      expectMsg(Finished(board, Some(p1.ref)))

    }

  }

  /*
  "An Ongoing match" should {

    "respond to Go" in new Case {
      mat ! Go // move out of starting state

      var board = Board()
      p1.expectMsg(Move(board))

      p1.reply(Claim(m1))

      board = board.successor(m1)
      mat ! Check
      expectMsg(Ongoing(board, p2.ref))



    }
  }*/

}
