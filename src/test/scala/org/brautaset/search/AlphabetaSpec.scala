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

    protected def ab(s: State, a: Double, b: Double, d: Int): Double = {
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

    def apply(state: State): Move = {

      def iter(mvs: Iterable[Move], m0: Move, a0: Double): Move = {
        if (mvs.isEmpty) m0
        else {
          val sc = -ab(state.successor(mvs.head), -100, -a0, depth-1)
          if (sc > a0) iter(mvs.tail, mvs.head, sc)
          else iter(mvs.tail, m0, a0)
        }
      }

      val moves = state.legalMoves
      if (moves.tail.isEmpty) moves.head
      else iter(moves, moves.head, -100)
    }

  }

  class VisitedAlphabeta(depth: Int, ffunc: (State) => Double) extends Alphabeta(depth, ffunc) {

    var visited = 0

    override def ab(s: State, a: Double, b: Double, d: Int): Double = {
      visited += 1
      super.ab(s, a, b, d)
    }

  }

}


class AlphabetaSpec extends WordSpec with MustMatchers {

  import AlphabetaSpec._

  trait Left {
    val lll = State(fitness = 7)
    val llr = State(fitness = -3)
    val lrl = State(fitness = -8)
    val lrr = State(fitness = 50)

    val ll = State(Map(Move(0) -> lll, Move(1) -> llr))
    val lr = State(Map(Move(2) -> lrl, Move(3) -> lrr))

    val left =  State(Map(Move(4) -> ll, Move(5) -> lr))
  }

  trait Right {

    val rll = State(fitness = 10)
    val rlr = State(fitness = -1)
    val rrl = State(fitness = 2)
    val rrr = State(fitness = 100)

    val rl = State(Map(Move(6) -> rll, Move(7) -> rlr))
    val rr = State(Map(Move(8) -> rrl, Move(9) -> rrr))

    val right = State(Map(Move(10) -> rl, Move(11) -> rr))
  }

  trait Both extends Left with Right {
    val origin = State(Map(Move(12) -> left, Move(13) -> right))
  }

  trait AB2 { val searcher = new VisitedAlphabeta(2, _.fitness) }
  trait AB9 { val searcher = new VisitedAlphabeta(9, _.fitness) }

  "search left to depth 2" should {

    "correctly identify best move" in new AB2 with Left {
      searcher(left) must be (Move(4))
    }

    "prune the last branch" in new AB2 with Left {
      searcher(left)
      searcher.visited must be (5)
    }

  }

  "search left to depth X" should {

    "correctly identify best move" in new AB9 with Left {
      searcher(left) must be (Move(4))
    }

    "prune the last branch" in new AB9 with Left {
      searcher(left)
      searcher.visited must be (5)
    }

  }

  "search right to depth 2" should {

    "correctly identify best move" in new AB2 with Right {
      searcher(right) must be (Move(11))
    }

    "fail to prune anything" in new AB2 with Right {
      searcher(right)
      searcher.visited must be (6)
    }

  }

  "search both to depth 3" should {

    "correctly identify best move" in new AB9 with Both {
      searcher(origin) must be (Move(12))
    }

    "prune some branches" in new AB9 with Both {
      searcher(origin)
      searcher.visited must be (10)
    }

  }

}
