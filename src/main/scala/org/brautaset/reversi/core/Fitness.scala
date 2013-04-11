package org.brautaset.reversi.core

object Fitness {

  def finish(board: Board): Double = {
    val turn = board.turn
    board.winner match {
      case None => 0.0
      case Some(`turn`) => Double.MaxValue
      case _ => -Double.MaxValue
    }
  }

  def mobility(board: Board): Double =
    board.legalMoves.size - board.legalMoves(board.turn.opponent).size

  def corner(board: Board): Double = {
    val corners = Set(Location(0, 0), Location(7, 7), Location(7, 0), Location(0, 7))
    val counts = board.captures.map { case (k, v) => k -> (v & corners).size }
    counts(board.turn) - counts(board.turn.opponent)
  }

  def capture(board: Board): Double =
    board.captures(board.turn).size - board.captures(board.turn.opponent).size

}

case class Fitness(capWgt: Double, mobWgt: Double, corWgt: Double) {
  import Fitness._

  def apply(board: Board) =
    if (board.isFinished) finish(board)
    else capWgt * capture(board) + mobWgt * mobility(board) + corWgt * corner(board)

}