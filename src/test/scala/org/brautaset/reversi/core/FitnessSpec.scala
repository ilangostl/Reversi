package org.brautaset.reversi.core

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class FitnessSpec extends WordSpec with MustMatchers {

  import Fitness._

  "finish" should {

    "return 0 for draws" in {
      finish(Board()) must be (0)
    }

    "return Int.MaxValue for wins of current turn" in {
      finish(Board(X, Map(X -> Set(Location(0, 0)), O -> Set()))) must be (Double.MaxValue)
    }

    "return -Int.MaxValue for wins of current turn" in {
      finish(Board(O, Map(X -> Set(Location(0, 0)), O -> Set()))) must be (Double.MinValue)
    }

  }

  "capture" should {

    "return 0 for initial board" in {
      capture(Board()) must be (0)
    }

    "be negative when current player is disadvantaged" in {
      capture(Board().successor(Location(4, 5))) must be (-3)
    }

    "be positive when current player has advantage" in {
      val board = Board().successor(Location(4, 5))
      capture(Board(X, board.captures)) must be (3)
    }

  }

  "corner" should {

    "score initial board 0" in {
      corner(Board()) must be (0)
    }

    "score board with corner" in {
      val board = Board(X, Map(X -> Set(Location(0, 0)), O -> Set()))
      corner(board) must be (1)
    }

    "score board with opponent's corner" in {
      val board = Board(X, Map(O -> Set(Location(0, 0)), X -> Set()))
      corner(board) must be (-1)
    }

    "score board with more than a corner" in {
      val board = Board(X, Map(X -> Set(Location(0, 0)), O -> Set(Location(1, 0))))
      corner(board) must be (1)
    }

    "score board with three corners" in {
      val board = Board(X, Map(X -> Set(Location(0, 0), Location(7, 7)), O -> Set(Location(7, 0))))
      corner(board) must be (1)
    }

  }

  "mobility" should {

    "be 0 originally" in {
      mobility(Board()) must be (0)
    }

    "be 0 when mobility is even, even if capture is not" in {
      val board = Board().successor(Location(4, 5))
      mobility(board) must be (0)
    }

    "be positive when current player has advantage" in {
      val board0 = Board().successor(Location(4, 5))
      val board1 = board0.successor(Location(5, 3))
      mobility(board1) must be (1)
    }

    "be negative when current player is disadvantaged" in {
      val board0 = Board().successor(Location(4, 5))
      val board1 = board0.successor(Location(5, 3))
      mobility(Board(O, board1.captures)) must be (-1)
    }

  }

  "fitness" should {

    "be 0 for initial board" in new Fitness(1, 1, 1) {
      fitness(Board()) must be (0)
    }

    "address weighted capture, corners and mobility" in new Fitness (1, 10, 100) {
      val board = Board(X, Map(X -> Set(Location(0, 0)), O -> Set(Location(1, 0), Location(1, 1))))
      fitness(board) must be (-1 + 10 + 100)
    }

  }

}
