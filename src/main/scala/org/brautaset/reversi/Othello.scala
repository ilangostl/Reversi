package org.brautaset.reversi

import annotation.tailrec
import core.{Fitness, Location, Board}

object Othello extends App {

  import Board._

  val Move = """(\d)\s*(\d)""".r

  val fitness = Fitness(1, 1, 1)

  val searcher = Alphabeta(fitness, 4)

  commandLoop(Board())

  @tailrec
  private def commandLoop(board: Board): Unit = {

    Console.readLine(s"$board\nYour move, please?\n> ") match {
      case "q" | "quit" => ()

      case "a" | "auto" =>
        auto(board)

      case Move(c, r) =>
        val move = Location(c.toInt, r.toInt)
        if (board.isLegalMoveDestination(move)) {
          commandLoop(board.successor(Occupy(move)))
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
    if (board.isFinished)
      println("GAME OVER")
    else
      auto(board.successor(searcher(board)))
  }

}
