package org.brautaset.reversi

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class PlayerSpec extends WordSpec with MustMatchers {

  "A Player" should {

    "compare equal to itself" in {
      X must be (X)
    }

    "compare not equal to itself" in {
      X must not be (O)
    }

    "know the opponent of X is O" in {
      Player.opponent(X) must be (O)
    }

    "know the opponent of O is X" in {
      Player.opponent(O) must be (X)
    }
  }
}
