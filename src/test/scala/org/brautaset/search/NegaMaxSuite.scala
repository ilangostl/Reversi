package org.brautaset.search

import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: SuperPappi
 * Date: 10/11/2012
 * Time: 21:48
 * To change this template use File | Settings | File Templates.
 */
class NegaMaxSuite extends FunSuite {

  trait TestData {
    val ttt = TicTacToe.apply
  }

  test("search ply 1 terminates") {
    new TestData {
      val m = NegaMax.search(ttt, 0)
    }
  }

}
