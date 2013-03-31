package org.brautaset.reversi.fitness

import org.brautaset.reversi.Board

object Mobility extends Fitness {

  def fitness(board: Board) =
    board.legalMoves.size - Board(board.turn.opponent, board.grid).legalMoves.size

}
