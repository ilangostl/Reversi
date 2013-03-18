package org.brautaset.reversi

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class BoardSpec extends WordSpec with MustMatchers {

  val locations = Map(Location('e', 4) -> X)

  "Board" should {

    "instantiate without arguments" in {
      val r = Board()
      r.currentPlayer must be (Board.firstPlayer)
      r.currentBoard must be (Board.initialLocations)
    }

    "instantiate with player" in {
      val r = Board(O)
      r.currentPlayer must be (O)
      r.currentBoard must be (Board.initialLocations)
    }

    "instantiate with player & board" in {
      val r = Board(O, Map.empty[Location,Player])
      r.currentPlayer must be (O)
      r.currentBoard must be (Map())
    }

  }

}
