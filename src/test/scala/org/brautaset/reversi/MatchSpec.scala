package org.brautaset.reversi

import akka.testkit.TestProbe
import akka.actor.Props
import core.{O, X, Location, Board}

class MatchSpec extends TestKitSpec("MatchSpec") {

  import Match._

  trait Case {
    val p1 = TestProbe()
    val p2 = TestProbe()
    val driver = TestProbe()
    val board = Board()
    val mat = system.actorOf(Props(new Match(driver.ref)))
    mat ! Start(p1.ref, p2.ref)
    driver.expectMsg(Ongoing(board, p1.ref))
  }

  "Start" should {

    "return initial state" in new Case {}

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

    "eventually cause game over" ignore new Case {

      val m = Map(X -> p1.ref, O -> p2.ref)
      def iter(moves: List[Location], board: Board, player: TestProbe, opponent: TestProbe): Board =
        if (moves.isEmpty)
          board
        else {
          mat.tell(Claim(moves.head), player.ref)
          iter(moves.tail, board.successor(moves.head), opponent, player)
        }

      val moves = List(
        Location(4, 5),
        Location(5, 3),
        Location(4, 2),
        Location(5, 5),
        Location(6, 4),
        Location(3, 5),
        Location(4, 6),
        Location(5, 4),
        Location(2, 4))

      val b = iter(moves, board, p1, p2)

      driver.expectMsgType[Ongoing]
      driver.expectMsg(Finished(b, Some(p1.ref)))

    }

  }

}
