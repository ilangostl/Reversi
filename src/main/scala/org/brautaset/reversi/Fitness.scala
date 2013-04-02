package org.brautaset.reversi

import core.{Location, Board}

object Fitness {

  def finish(board: Board) = {
    val turn = board.turn
    board.winner match {
      case None => 0
      case Some(`turn`) => Int.MaxValue
      case _ => -Int.MaxValue
    }
  }

  def mobility(board: Board) =
    board.legalMoves.size - board.legalMoves(board.turn.opponent).size

  def corner(board: Board) = {
    val corners = Set(Location(0, 0), Location(7, 7), Location(7, 0), Location(0, 7))
    val counts = board.captures.map { case (k, v) => k -> (v & corners).size }
    counts(board.turn) - counts(board.turn.opponent)
  }

  def capture(board: Board) =
    board.captures(board.turn).size - board.captures(board.turn.opponent).size

}
