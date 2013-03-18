package org.brautaset.reversi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class BoardSpec extends WordSpec with MustMatchers {

  "A Board's constructor" should {

    "use defaults when invoked with no arguments" in {
      val b = Board()
      b.playerTurn must be (X)
      b.grid must be (Board.initialGrid)
    }

    "accept player & grid" in {
      val r = Board(O, Map.empty[Location,Player])
      r.playerTurn must be (O)
      r.grid must be (Map())
    }
  }

  "A Board" should {

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
      board.unoccupiedNeighboursToOpponent must have size (unoccupiedNeighbours.size)
      board.unoccupiedNeighboursToOpponent must be (unoccupiedNeighbours)
    }

    "not return unoccupied neighbour locations outside the board" in {
      val b = Board(X, Map(Location(0, 6) -> X, Location(0, 7) -> O))
      b.unoccupiedNeighboursToOpponent must be(Set(Location(1, 6), Location(1, 7)))
    }

    "return no neighbours for unpopulated grid" in {
      val b = Board(X, Map.empty[Location,Player])
      b.unoccupiedNeighboursToOpponent must be (Set())
    }

  }

}
