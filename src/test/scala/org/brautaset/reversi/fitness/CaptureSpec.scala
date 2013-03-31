package org.brautaset.reversi.fitness

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import org.brautaset.reversi.{X, Location, Board}

class CaptureSpec extends WordSpec with MustMatchers {

  "fitness" should {

    "return 0 for initial board" in {
      Capture.fitness(Board()) must be (0)
    }

    "be negative when current player is disadvantaged" in {
      Capture.fitness(Board().successor(Location(4, 5))) must be (-3)
    }

    "be positive when current player has advantage" in {
      val board = Board().successor(Location(4, 5))
      Capture.fitness(Board(X, board.grid)) must be (3)
    }


  }

}
