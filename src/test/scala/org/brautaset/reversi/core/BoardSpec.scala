package org.brautaset.reversi.core

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class BoardSpec extends WordSpec with MustMatchers {

  import Board._

  "A Board's constructor" should {

    "use defaults when invoked with no arguments" in {
      val b = Board()
      b.turn must be (X)
      b.captures must be (Board.initialCaptures)
    }

    "accept player & captures" in {
      val r = Board(O, Map(X -> Set(), O -> Set()))
      r.turn must be (O)
      r.captures must be (Map(X -> Set(), O -> Set()))
    }
  }

  "toString" should {

    "represent board suitably during game" in {
      Board().toString must be (
        """
          |/01234567
          |0........
          |1........
          |2........
          |3...OX...
          |4...XO...
          |5........
          |6........
          |7........
          |X to move""".stripMargin)
    }

    "represent board suitably when finished" in {
      Board(O, Map(X -> Set(Location(4, 4)), O -> Set())).toString must be (
        """
          |/01234567
          |0........
          |1........
          |2........
          |3........
          |4....X...
          |5........
          |6........
          |7........
          |X won""".stripMargin)
    }

    "represent board suitably when drawn" in {
      Board(O, Map(O -> Set(Location(7,0)), X -> Set(Location(4,4)))).toString must be (
        """
          |/01234567
          |0.......O
          |1........
          |2........
          |3........
          |4....X...
          |5........
          |6........
          |7........
          |Draw""".stripMargin)
    }

  }

  "isLegalMove" should {

    "know that [4,5] is a legal move" in {
      Board().isLegalMove(Location(4, 5)) must be (true)
    }
  }

  "legalMoves" should {
    "be 4 initially" in {
      Board().legalMoves must be (Set(Location(4,5), Location(5,4), Location(3,2), Location(2,3)).map(Occupy(_)))
    }
  }

  "successor" should {

    "update turn" in {
      Board().successor(Occupy(Location(5, 4))).turn must be (O)
    }

    "update captures to occupy 1 more location" in {
      Board().successor(Occupy(Location(5, 4))).captures.values.flatten must have size (5)
    }

    "update captures to occupy move location" in {
      val loc = Location(5,4)
      val captures = Map(
        X -> Set(Location(3,4), Location(4,4), Location(5,4), Location(4,3)),
        O -> Set(Location(3,3)))
      Board().successor(loc).captures must be (captures)
    }

    "throw if attempting to make illegal move" in {
      evaluating {
        Board().successor(Location(0, 0))
      } must produce [IllegalArgumentException]

    }

    "not accept pass if player can move" in {
      intercept[IllegalArgumentException] {
        Board().successor(Pass)
      }
    }

    "not accept pass if finished" in {
      intercept[IllegalArgumentException] {
        Board(X, Map(O -> Set(), X -> Set())).successor(Pass)
      }
    }

    "flip the player turn" in {
      val a = Board(X, Map(O -> Set(Location(0, 0)), X -> Set(Location(0, 1))))
      val b = a.successor(Pass)
      b.turn must be (a.turn.opponent)
    }

    "retain the captures" in {
      val a = Board(X, Map(O -> Set(Location(0, 0)), X -> Set(Location(0, 1))))
      val b = a.successor(Pass)
      b.captures must be (a.captures)
    }
  }

  "isFinished" should {

    "start out false" in {
      Board().isFinished must be (false)
    }

    "account for passes" in {
      val board = Board(X, Map(O -> Set(Location(0, 0)), X -> Set(Location(0, 1))))
      board.isFinished must be (false)
    }

    "eventually become true" in {
      def iter(board: Board): Boolean =
        if (board.isFinished) true
        else iter(board.successor(board.legalMoves.head))
      iter(Board()) must be (true)
    }

    "be possible after a few moves" in {
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

      def iter(board: Board, moves: List[Location]): Board =
        if (moves.isEmpty) board
        else iter(board.successor(moves.head), moves.tail)

      iter(Board(), moves).isFinished must be (true)
    }

  }

  "winner" should {
    "return None in case of a draw" in {
      Board().winner must be (None)
    }

    "return as winner the side with most tiles" in {
      Board().successor(Location(5, 4)).winner must be (Some(X))
    }

    "not be impacted by who's turn it is" in {
      val board = Board().successor(Location(5, 4))
      // rewind turn
      Board(board.turn.opponent, board.captures).winner must be (Some(X))
    }

  }

}
