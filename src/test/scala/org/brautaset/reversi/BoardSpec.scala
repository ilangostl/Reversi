package org.brautaset.reversi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class BoardSpec extends WordSpec with MustMatchers {

  val locations = Map(Location(5, 4) -> X)

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

    "find correct number of neighbours for initial grid" in {
      Board().neighboursOfOccupiedLocations must have size (12)
    }

    "not return neighbours outside the board" in {
      val b = Board(X, Map(Location(0, 6) -> X, Location(0, 7) -> O))
      b.neighboursOfOccupiedLocations must be(Set(Location(0, 5), Location(1, 5), Location(1, 6), Location(1, 7)))
    }

    "return no neighbours for unpopulated grid" in {
      val b = Board(X, Map.empty[Location,Player])
      b.neighboursOfOccupiedLocations must be (Set())
    }

  }

}
