package org.brautaset.reversi

import org.brautaset.reversi.core.{Board, Fitness}

import annotation.tailrec

case class Alphabeta(fitness: Fitness, depth: Int) {
  require(depth > 0)

  import Board.Move

  def apply(board: Board): Move = {

    def ab(s: Board, a: Double, b: Double, d: Int): Double = {
      if (s.isFinished || d <= 0)
        fitness(s)
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


    def iter(mvs: Iterable[Move], m0: Move, a0: Double): Move = {
      if (mvs.isEmpty) m0
      else {
        val sc = -ab(board.successor(mvs.head), -100, -a0, depth-1)
        if (sc > a0) iter(mvs.tail, mvs.head, sc)
        else iter(mvs.tail, m0, a0)
      }
    }

    val moves = board.legalMoves
    if (moves.tail.isEmpty) moves.head
    else iter(moves, moves.head, -100)
  }

}
