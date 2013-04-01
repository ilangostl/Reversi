package org.brautaset.reversi.core

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class SideSpec extends WordSpec with MustMatchers {

  "A Side" should {

    "know its opponent" in {
      X.opponent must be (O)
    }

    "know its opponent (2)" in {
      O.opponent must be (X)
    }

    "know that its opponent's opponent is itself" in {
      O.opponent.opponent must be (O)
    }

  }
}
