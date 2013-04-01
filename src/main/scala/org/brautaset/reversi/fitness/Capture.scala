package org.brautaset.reversi.fitness

import org.brautaset.reversi.Board

trait Capture {

  def captureFitness(board: Board) = {
    val p = board.turn
    board.grid.values.collect {
      case `p` => 1
      case _ => -1
    }.sum
  }

}

