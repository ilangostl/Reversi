package org.brautaset.reversi

object Negamax {

  def negamax(fitness: (Board) => Int)(board: Board, plyLeft: Int) = {
    require(!board.isFinished)
    require(plyLeft > 0)

    def iter(board: Board, plyLeft: Int): Int =
      if (board.isFinished)
        -board.finishScore
      else if (plyLeft <= 0)
        -fitness(board)
      else
        board.legalMoves.map(m => m -> -iter(board.successor(m), plyLeft - 1)).toMap.maxBy(_._2)._2

    val scores = board.legalMoves.par.map(m => m -> iter(board.successor(m), plyLeft - 1)).toMap
    // println(scores)
    scores.maxBy(_._2)._1
  }

}
