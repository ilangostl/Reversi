package org.brautaset.reversi

import core.{Location, Board}

object Fitness {

  def mobility(board: Board) =
    board.legalMoves.size - Board(board.turn.opponent, board.grid).legalMoves.size

  def corner(board: Board) = {
    val corners = List(Location(0, 0), Location(7, 7), Location(7, 0), Location(0, 7))
    val p = board.turn
    corners.flatMap(c => board.grid.get(c)).collect {
      case `p` => 1
      case _ => -1
    }.sum
  }

  def capture(board: Board) = {
    val p = board.turn
    board.grid.values.collect {
      case `p` => 1
      case _ => -1
    }.sum
  }

}
