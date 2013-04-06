package org.brautaset.reversi

import core.{Fitness, Board}

case class Negamax(fitness: Fitness, ply: Int) {
  require(ply > 0)

  def apply(board: Board) = {
    require(!board.isFinished)

    def iter(board: Board, plyLeft: Int): Double =
      if (board.isFinished || plyLeft <= 0)
        -fitness.fitness(board)
      else
        board.legalMoves.map(m => -iter(board.successor(m), plyLeft - 1)).max

    val moves = board.legalMoves
    if (moves.tail.isEmpty)
      moves.head
    else
      moves.par.map(m => m -> iter(board.successor(m), ply - 1)).toMap.maxBy(_._2)._1
  }

}
