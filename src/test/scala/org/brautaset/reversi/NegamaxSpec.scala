package org.brautaset.reversi

import core._
import org.scalatest.matchers.MustMatchers
import org.scalatest.WordSpec

class NegamaxSpec extends WordSpec with MustMatchers {

  import Board._

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
    else iter(board.successor(Occupy(moves.head)), moves.tail)

  def nm(board: Board, ply: Int) = Negamax(Fitness(1, 0, 0).fitness)(board, ply)

  "Negamax" should {

    "not accept finished boards" in {
      intercept[IllegalArgumentException] {
        nm(Board(X, Map(X -> Set.empty, O -> Set.empty)), 1)
      }
    }

    "not accept ply-left of 0 or less" in {
      intercept[IllegalArgumentException] {
        nm(Board(), 0)
      }
    }

    "find winning move at ply 1" in {
      val board = iter(Board(), winningMoves.take(8))
      nm(board, 1) must be (Occupy(winningMoves(8)))
    }

    "avoid losing move at ply 2" in {
      val board = iter(Board(), winningMoves.take(7))
      nm(board, 2) must not be (Occupy(winningMoves(7)))
    }

    "be able to play a game to the end" in {

      val nm = Negamax(Fitness(1, 1, 1).fitness) _

      def iter(board: Board): Board =
        if (board.isFinished) board
        else iter(board.successor(nm(board, 2)))

      val board = iter(Board())
      board.isFinished must be (true)
    }

  }

}
