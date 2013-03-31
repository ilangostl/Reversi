package org.brautaset.reversi.fitness

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import org.brautaset.reversi.{O, Location, X, Board}

class CornerCaptureSpec extends WordSpec with MustMatchers {

  "cornerCaptureFitness" should {

    "score initial board 0" in new CornerCapture {
      cornerCaptureFitness(Board()) must be (0)
    }

    "score board with corner" in new CornerCapture {
      val board = Board(X, Map(Location(0, 0) -> X))
      cornerCaptureFitness(board) must be (3)
    }

    "score board with opponent's corner" in new CornerCapture {
      val board = Board(O, Map(Location(0, 0) -> X))
      cornerCaptureFitness(board) must be (-3)
    }

    "score board with more than a corner" in new CornerCapture {
      val board = Board(X, Map(Location(0, 0) -> X, Location(1, 0) -> O))
      cornerCaptureFitness(board) must be (3)
    }

    "score board with three corners" in new CornerCapture {
      val board = Board(X, Map(Location(0, 0) -> X, Location(7, 0) -> O, Location(7, 7) -> X))
      cornerCaptureFitness(board) must be (3)
    }

  }

}
