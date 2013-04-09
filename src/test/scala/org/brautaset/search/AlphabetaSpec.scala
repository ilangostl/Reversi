package org.brautaset.search

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

object AlphabetaSpec {

  case class Move(i: Int)
  case class State(children: Map[Move, State] = Map(), fitness: Double = 0) {
    def successor(m: Move) = children(m)
    val legalMoves = children.keys
    val isFinished = children.isEmpty
  }

  class Alphabeta(depth: Int, ffunc: (State) => Double) {
    require(depth > 0)

    def apply(state: State): Move = {

      def ab(s: State, a: Double, b: Double, d: Int): Double =
        if (s.isFinished || d <= 0)
          ffunc(s)
        else
          s.legalMoves.map(m => -ab(s.successor(m), -b, -a, d - 1)).max

      state.legalMoves.map(m => m -> -ab(state.successor(m), Double.MinValue, Double.MaxValue, depth - 1)).maxBy(_._2)._1
    }

  }

}


class AlphabetaSpec extends WordSpec with MustMatchers {

  import AlphabetaSpec._

  trait TwoLevel {
    val ll = State(fitness = 7)
    val lr = State(fitness = 3)
    val rl = State(fitness = -8)
    val rr = State(fitness = 50)

    val left = State(Map(Move(0) -> ll, Move(1) -> lr))
    val right = State(Map(Move(2) -> rl, Move(3) -> rr))
    val origin = State(Map(Move(4) -> left, Move(5) -> right))

    val ab = new Alphabeta(2, _.fitness)
  }

  "apply" should {

    "correctly identify best move" in new TwoLevel {
      ab(origin) must be (Move(4))
    }

  }


}
