package org.brautaset.reversi.fitness

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import org.brautaset.reversi.{O, X, Location, Board}

class MobilitySpec extends WordSpec with MustMatchers {

  "fitness" should {

    "be 0 originally" in new Mobility {
      mobilityFitness(Board()) must be (0)
    }

    "be 0 when mobility is even, even if capture is not" in new Mobility {
      val board = Board().successor(Location(4, 5))
      mobilityFitness(board) must be (0)
    }

    "be positive when current player has advantage" in new Mobility {
      val board0 = Board().successor(Location(4, 5))
      val board1 = board0.successor(Location(5, 3))
      mobilityFitness(board1) must be (1)
    }

    "be negative when current player is disadvantaged" in new Mobility {
      val board0 = Board().successor(Location(4, 5))
      val board1 = board0.successor(Location(5, 3))
      mobilityFitness(Board(O, board1.grid)) must be (-1)
    }

  }

}
