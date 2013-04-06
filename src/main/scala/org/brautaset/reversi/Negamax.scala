package org.brautaset.reversi

import core.Board

object Negamax {

  def apply(fitness: (Board) => Double)(board: Board, plyLeft: Int) = {
    require(!board.isFinished)
    require(plyLeft > 0)

    def iter(board: Board, plyLeft: Int): Double =
      if (board.isFinished || plyLeft <= 0)
        -fitness(board)
      else
        board.legalMoves.map(m => m -> -iter(board.successor(m), plyLeft - 1)).toMap.maxBy(_._2)._2

    val moves = board.legalMoves
    if (moves.tail.isEmpty)
      moves.head
    else
      moves.par.map(m => m -> iter(board.successor(m), plyLeft - 1)).toMap.maxBy(_._2)._1
  }

}
