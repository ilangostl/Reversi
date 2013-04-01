package org.brautaset.reversi.fitness

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import org.brautaset.reversi.{X, Location, Board}

class CaptureSpec extends WordSpec with MustMatchers {

  "fitness" should {

    "return 0 for initial board" in new Capture {
      captureFitness(Board()) must be (0)
    }

    "be negative when current player is disadvantaged" in new Capture {
      captureFitness(Board().successor(Location(4, 5))) must be (-3)
    }

    "be positive when current player has advantage" in new Capture {
      val board = Board().successor(Location(4, 5))
      captureFitness(Board(X, board.grid)) must be (3)
    }


  }

}
