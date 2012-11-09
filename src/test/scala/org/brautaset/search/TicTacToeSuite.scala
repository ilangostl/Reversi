package org.brautaset.search

import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: SuperPappi
 * Date: 08/11/2012
 * Time: 21:25
 * To change this template use File | Settings | File Templates.
 */
class TicTacToeSuite extends FunSuite {

  trait TTT {
    val ttt = TicTacToe(Cross)
  }


  test("toString") {
    new TTT {
      val expect = List("---", "---", "---").mkString("\n")
      assert(ttt.toString === expect)
    }
  }

  test("successor") {
    new TTT {
      val expect = List("x--", "---", "---").mkString("\n")

      val s1 = ttt.successor(Slot(0, 0))
      assert(s1.toString === expect)

      val expect2 = List("x--", "---", "--o").mkString("\n")

      val s2 = s1.successor(Slot(2, 2))
      assert(s2.toString === expect2)
    }
  }

  test("legalMoves") {
    new TTT {
      val expect = {
        for {
          x <- 0 to 2
          y <- 0 to 2
        } yield Slot(x, y)
      }.toSet
      assert(expect === ttt.legalMoves)
    }
  }

  test("fitness") {
    new TTT {
      assert(ttt.fitness === 0)

      val s1 = ttt.successor(Slot(0, 0))
//      assert(s1.fitness === -3)
    }
  }


}
