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

  test("toString - at initial position") {
    val expect = """|---
                    |---
                    |---""".stripMargin
    assert(expect === TicTacToe(Cross).toString)
    assert(expect === TicTacToe(Circle).toString)
  }

  test("toString - after a move") {
    val expect =
      """|x--
         |---
         |---""".stripMargin

    val s1 = TicTacToe(Cross).successor(Slot(0,0))
    assert(expect === s1.toString)

    val expect2 =
      """|x--
         |---
         |--o""".stripMargin

    val s2 = s1.successor(Slot(2,2))
    assert(expect2 === s2.toString)
  }

}
