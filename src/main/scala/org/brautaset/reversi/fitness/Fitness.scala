package org.brautaset.reversi.fitness

import org.brautaset.reversi.Board

trait Fitness {

  def fitness(board: Board): Int

}
