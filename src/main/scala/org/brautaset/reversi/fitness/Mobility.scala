package org.brautaset.reversi.fitness

import org.brautaset.reversi.Board

trait Mobility {

  def mobilityFitness(board: Board) =
    board.legalMoves.size - Board(board.turn.opponent, board.grid).legalMoves.size

}
