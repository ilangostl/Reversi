package org.brautaset.search

import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec
import scala.annotation.tailrec

object AlphabetaSpec {

  case class Move(i: Int)
  case class State(children: Map[Move, State] = Map(), fitness: Double = 0) {
    def successor(m: Move) = children(m)
    val legalMoves = children.keys
    val isFinished = children.isEmpty
  }

  class Alphabeta(depth: Int, ffunc: (State) => Double) {
    require(depth > 0)

    var visited = 0

    def apply(state: State): Move = {

      def ab(s: State, a: Double, b: Double, d: Int): Double = {
  //      println(s.fitness, a, b)
        visited += 1
        if (s.isFinished || d <= 0)
          ffunc(s)
        else {
          @tailrec
          def iter(mv: Iterable[Move], a0: Double): Double =
            if (mv.isEmpty) a0
            else {
              val sc = -ab(s.successor(mv.head), -b, -a0, d - 1)
              if (sc >= b) sc
              else iter(mv.tail, if (sc > a0) sc else a0)
            }
          iter(s.legalMoves, a)
        }
      }

      def outer(mvs: Iterable[Move], m0: Move, a0: Double): Move = {
//        println(m0 -> a0)
        if (mvs.isEmpty) m0
        else {
          val sc = -ab(state.successor(mvs.head), -100, -a0, depth-1)
          println("Sc: " + sc)
          if (sc > a0) outer(mvs.tail, mvs.head, sc)
          else outer(mvs.tail, m0, a0)
        }
      }

      outer(state.legalMoves, state.legalMoves.head, -100)
    }

  }

}


class AlphabetaSpec extends WordSpec with MustMatchers {

  import AlphabetaSpec._

  trait TwoLevel {
    val ll = State(fitness = 7)
    val lr = State(fitness = -3)
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

    "prune the last branch" in new TwoLevel {
      ab(origin)
      ab.visited must be (5)
    }

  }


}
