package org.brautaset.reversi

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class LocationSpec extends WordSpec with MustMatchers {

  "A Location's neighbours" should {

    "always be 8" in {
      Location('a',0).neighbours must have size (8)
    }

    "always be 8 (2)" in {
      Location('e',5).neighbours must have size (8)
    }

    "not contain itself" in {
      val loc = Location('e', 3)
      loc.neighbours must not contain (loc)
    }

  }

}
