package org.brautaset.reversi.fitness

import org.brautaset.reversi.{Location, Board}



trait Corners {

  private val corners = List(Location(0, 0), Location(7, 7), Location(7, 0), Location(0, 7))

  def cornerFitness(board: Board) = {
    val p = board.turn
    corners.flatMap(c => board.grid.get(c)).collect {
      case `p` => 1
      case _ => -1
    }.sum
  }

}
