package org.brautaset.reversi

import annotation.tailrec

object Othello extends App {

  val Move = """(\d)\s*(\d)""".r

  commandLoop(Board())

  @tailrec
  private def commandLoop(board: Board): Unit = {

    Console.readLine(s"$board\nYour move, please?\n> ") match {
      case "q" | "quit" => ()

      case "a" | "auto" =>
        auto(board)

      case Move(c, r) =>
        val move = Location(c.toInt, r.toInt)
        if (board.isLegalMove(move)) {
          commandLoop(board.successor(move))
        } else {
          println("Illegal move!")
          commandLoop(board)
        }

      case x =>
        println(s"Not a move: [$x]")
        commandLoop(board)

    }
  }

  private def auto(board: Board): Unit = {
    println(board)
    if (board.legalMoves.isEmpty)
      println("GAME OVER")
    else
      auto(board.successor(board.legalMoves.head))
  }

}
