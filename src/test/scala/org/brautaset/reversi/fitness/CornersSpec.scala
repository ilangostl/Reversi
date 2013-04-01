package org.brautaset.reversi.fitness

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import org.brautaset.reversi.{O, Location, X, Board}

class CornersSpec extends WordSpec with MustMatchers {

  "cornerFitness" should {

    "score initial board 0" in new Corners {
      cornerFitness(Board()) must be (0)
    }

    "score board with corner" in new Corners {
      val board = Board(X, Map(Location(0, 0) -> X))
      cornerFitness(board) must be (1)
    }

    "score board with opponent's corner" in new Corners {
      val board = Board(O, Map(Location(0, 0) -> X))
      cornerFitness(board) must be (-1)
    }

    "score board with more than a corner" in new Corners {
      val board = Board(X, Map(Location(0, 0) -> X, Location(1, 0) -> O))
      cornerFitness(board) must be (1)
    }

    "score board with three corners" in new Corners {
      val board = Board(X, Map(Location(0, 0) -> X, Location(7, 0) -> O, Location(7, 7) -> X))
      cornerFitness(board) must be (1)
    }

  }

}
