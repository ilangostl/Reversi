package org.brautaset.reversi

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class LocationSpec extends WordSpec with MustMatchers {

  "Locations" should {

    "know who their neighbours are" in {
      Location('a',0).neighbours must be(Set(Location('b',0), Location('b', 1), Location('a', 1)))
    }

    "know the neighbours for a set of locations" in {
      val input = Set(Location('a', 0), Location('a', 1))
      Location.neighbours(input) must be(Set(Location('a', 2), Location('b', 0), Location('b', 1), Location('b', 2)))
    }

  }



}
