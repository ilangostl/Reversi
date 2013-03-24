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
      val r = Board(O, Map.empty[Location,Piece])
      r.turn must be (O)
      r.grid must be (Map())
    }
  }

  "A Board" should {

    "stringify sensibly" in {
      Board().toString must be (
        """/01234567
          |0........
          |1........
          |2........
          |3...XO...
          |4...OX...
          |5........
          |6........
          |7........
          |X to move""".stripMargin)
    }

    "know locations held by opponent" in {
      val board = Board()
      board.locationsHeldByOpponent must have size (2)
      board.locationsHeldByOpponent must be (Set(Location(3, 4), Location(4, 3)))
    }

    "know unoccupied neighbours for locations held by opponent" in {
      val unoccupiedNeighbours = Set(
        Location(2,3), Location(2,4), Location(2,5),
        Location(3,2), Location(3,5),
        Location(4,2), Location(4,5),
        Location(5,2), Location(5,3), Location(5,4))

      val board = Board()
      board.unoccupiedNeighboursToLocationsHeldByOpponent must have size (unoccupiedNeighbours.size)
      board.unoccupiedNeighboursToLocationsHeldByOpponent must be (unoccupiedNeighbours)
    }

    "not return unoccupied neighbour locations outside the board" in {
      val b = Board(X, Map(Location(0, 6) -> X, Location(0, 7) -> O))
      b.unoccupiedNeighboursToLocationsHeldByOpponent must be(Set(Location(1, 6), Location(1, 7)))
    }

    "return no neighbours for unpopulated grid" in {
      val b = Board(X, Map.empty[Location,Piece])
      b.unoccupiedNeighboursToLocationsHeldByOpponent must be (Set())
    }

    "knows that [5,3] is a legal move" in {
      Board().isLegalMove(Location(5, 3)) must be (true)
    }

    "know the legal moves" in {
      Board().legalMoves must be (Set(Location(3,5), Location(4,2), Location(2,4), Location(5,3)))
    }

    "find flipped locations for a move" in {
      Board().locationsFlippedByMove(Location(5, 3)) must be (Set(Location(4, 3)))
    }
  }

  "A Board's successor" should {

    "update turn" in {
      Board().successor(Location(5, 3)).turn must be (O)
    }

    "update grid to occupy 1 more location" in {
      Board().successor(Location(5, 3)).grid must have size (5)
    }

    "update grid to occupy move location" in {
      val loc = Location(5, 3)
      val grid = Map(
        Location(3, 3) -> X,
        Location(3, 4) -> O,
        Location(4, 3) -> X,
        Location(4, 4) -> X,
        Location(5, 3) -> X)
      Board().successor(loc).grid must be (grid)
    }

  }

}
