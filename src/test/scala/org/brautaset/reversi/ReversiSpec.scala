package org.brautaset.reversi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class ReversiSpec extends WordSpec with MustMatchers {

  import Reversi._

  "A Player" should {

    "know the starting player" in {
      Player.first must be (X)
    }

    "know the opponent of X is O" in {
      Player.opponent(X) must be (O)
    }

    "know the opponent of O is X" in {
      Player.opponent(O) must be (X)
    }
  }

  "Reversi" should {

    "instantiate without arguments" in {
      val r = Reversi()
      r.currentPlayer must be (X)
      r.currentBoard must be (initialBoard)
    }

    "instantiate with player" in {
      val r = Reversi(O)
      r.currentPlayer must be (O)
      r.currentBoard must be (initialBoard)
    }

    "instantiate with board" in {
      val r = Reversi(Map.empty[Location,Player])
      r.currentPlayer must be (X)
      r.currentBoard must be (Map())
    }

    "instantiate with player & board" in {
      val r = Reversi(O, Map.empty[Location,Player])
      r.currentPlayer must be (O)
      r.currentBoard must be (Map())
    }

  }

  "Locations" should {

    "know who their neighbours are" in {
      Location('a',0).neighbours must be(Set(Location('b',0), Location('b', 1), Location('a', 1)))
    }

    "know the neighbours for a set of locations" in {
      val input = Set(Location('a', 0), Location('a', 1))
      Location.neighbours(input) must be(Set(Location('a', 2), Location('b', 0), Location('b', 1), Location('b', 2)))
    }

  }

}
