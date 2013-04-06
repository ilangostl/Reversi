package org.brautaset.reversi

import core.{Board, Fitness}
import annotation.tailrec

case class Alphabeta(fitness: Fitness, ply: Int) {
  require(ply > 0)

  def apply(board: Board) = {
    require(!board.isFinished)

    def iter(board: Board, alpha: Double, beta: Double, plyLeft: Int): Double =
      if (board.isFinished || plyLeft <= 0)
        fitness.fitness(board)
      else {
        @tailrec
        def iter0(moves: Iterable[Board.Move], a: Double): Double = {
          if (moves.isEmpty)
            a
          else {
            val sc = -iter(board.successor(moves.head), -beta, -a, plyLeft - 1)
            if (sc >= beta)
              sc
            else
              iter0(moves.tail, if (sc > a) sc else a)
          }
        }

        iter0(board.legalMoves, alpha)
      }

    val moves = board.legalMoves
    if (moves.tail.isEmpty)
      moves.head
    else {
      val alpha = Double.MaxValue
      val beta = -iter(board.successor(moves.head), alpha, Double.MaxValue, ply - 1)
      val scores = moves.tail.par.map(m => m -> -iter(board.successor(m), -beta, -alpha, ply - 1)).toMap
      (scores + (moves.head -> beta)).maxBy(_._2)._1
    }
  }


}
