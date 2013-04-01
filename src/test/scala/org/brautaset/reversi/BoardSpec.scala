package org.brautaset.reversi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class BoardSpec extends WordSpec with MustMatchers {

  "A Board's constructor" should {

    "use defaults when invoked with no arguments" in {
      val b = Board()
      b.turn must be (X)
      b.grid must be (Board.initialGrid)
    }

    "accept player & grid" in {
      val r = Board(O, Map.empty[Location,Side])
      r.turn must be (O)
      r.grid must be (Map())
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
      Board(O, Map(Location(4,4) -> X)).toString must be (
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
      Board(O, Map(Location(7,0) -> O, Location(4,4) -> X)).toString must be (
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
      Board().legalMoves must be (Set(Location(4,5), Location(5,4), Location(3,2), Location(2,3)))
    }
  }

  "successor" should {

    "update turn" in {
      Board().successor(Location(5, 4)).turn must be (O)
    }

    "update grid to occupy 1 more location" in {
      Board().successor(Location(5, 4)).grid must have size (5)
    }

    "update grid to occupy move location" in {
      val loc = Location(5, 4)
      val grid = Map(
        Location(3,4) -> X,
        Location(4,4) -> X,
        Location(5,4) -> X,
        Location(3,3) -> O,
        Location(4,3) -> X)
      Board().successor(loc).grid must be (grid)
    }

    "throw if attempting to make illegal move" in {
      evaluating {
        Board().successor(Location(0, 0))
      } must produce [IllegalArgumentException]

    }

  }

  "isFinished" should {

    "start out false" in {
      Board().isFinished must be (false)
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

  "finishScore" should {

    "return 0 for draws" in {
      Board().finishScore must be (0)
    }

    "return Int.MaxValue for wins of current turn" in {
      Board(X, Map(Location(0, 0) -> X)).finishScore must be (Int.MaxValue)
    }

    "return -Int.MaxValue for wins of current turn" in {
      Board(O, Map(Location(0, 0) -> X)).finishScore must be (-Int.MaxValue)
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
      Board(board.turn.opponent, board.grid).winner must be (Some(X))
    }

  }

}
