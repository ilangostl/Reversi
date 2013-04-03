package org.brautaset.reversi.core

import org.scalatest.WordSpec
import org.scalatest.matchers.MustMatchers

class DirectionSpec extends WordSpec with MustMatchers {

  import Direction._

  "all" should {

    "contain 8 objects" in {
      all must have size (8)
    }

    "contain only uniques" in {
      all must be (all.toSet)
    }

    "not contain 0,0 as a direction" in {
      all must not contain (Direction(0, 0))
    }

  }

}
