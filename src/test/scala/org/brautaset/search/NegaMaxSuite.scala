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
    val s = new NegaMax {
        type M = Point
    }
  }

  test("search ply 0 throws") {
    new TestData {
      intercept[IllegalArgumentException] {
        s.search(ttt, 0)
      }
    }
  }

  test("search ply 1 terminates") {
    new TestData {
      val m = s.search(ttt, 1)
    }
  }

  test("search ply 2 terminates") {
    new TestData {
      val m = s.search(ttt, 2)
    }
  }

  test("search ply 9 terminates.. slowly") {
    new TestData {

      val m = s.search(ttt, 9)
      println(ttt + " => " + m)


      val t1 = ttt.successor(m)
      println(t1)

      val m1 = s.search(t1, 9)
      println(t1 + " => " + m1)

      val t2 = t1.successor(m1)
      val m2 = s.search(t2, 9)
      println(t2 + " => " + m2)

    }
  }



}
