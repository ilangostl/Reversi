package org.brautaset.reversi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class ReversiSpec extends WordSpec with MustMatchers {

  import Reversi._

  "Player X" should {

    "be the starting player" in {
      Player.first must be (X)
    }

    "know its opponent is O" in {
      Player.opponent(X) must be (O)
    }
  }

  "Player O" should {
    "know its opponent is X" in {
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

  "A location" should {

    "know who its neighbours are" in {
      Reversi.neighbours(Location('a',0)) must be(Set(Location('b',0), Location('b', 1), Location('a', 1)))
    }

  }

}
