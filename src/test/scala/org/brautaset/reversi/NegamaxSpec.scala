package org.brautaset.reversi

import fitness.{Capture, Fitness}
import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class NegamaxSpec extends WordSpec with MustMatchers {

  val fit = new Fitness with Capture {
    def fitness(board: Board) = captureFitness(board)
  }

  val winningMoves = List(
    Location(4, 5),
    Location(5, 3),
    Location(4, 2),
    Location(5, 5),
    Location(6, 4),
    Location(3, 5),
    Location(4, 6),
    Location(5, 4),
    Location(2, 4))

  def iter(board: Board, moves: List[Location]): Board =
    if (moves.isEmpty) board
    else iter(board.successor(moves.head), moves.tail)


  def nm(board: Board, ply: Int) = Negamax.negamax(fit)(board, ply)

  "negamax" should {

    "not accept finished boards" in {
      intercept[IllegalArgumentException] {
        nm(Board(X, Map()), 1)
      }
    }

    "not accept ply-left of 0 or less" in {
      intercept[IllegalArgumentException] {
        nm(Board(), 0)
      }
    }

    "find winning move at ply 1" in {
      val board = iter(Board(), winningMoves.take(8))
      nm(board, 1) must be (winningMoves(8))
    }

    "avoid losing move at ply 2" in {
      val board = iter(Board(), winningMoves.take(7))
      nm(board, 2) must not be (winningMoves(7))
    }

  }

}
