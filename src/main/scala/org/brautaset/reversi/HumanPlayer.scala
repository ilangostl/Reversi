package org.brautaset.reversi

import akka.actor.{ActorLogging, Actor}
import core.Board.Occupy
import core.{Location, Board}
import annotation.tailrec

class HumanPlayer extends Actor with ActorLogging {

  import Match._

  def receive = {
    case Move(board) =>
      // Only 1 legal move? Take it for me.
      if (board.legalMoves.tail.isEmpty) {
        log.debug("Only 1 move available..")
        sender ! Claim(board.legalMoves.head)
      }
      else
        sender ! commandLoop(board)
  }

  @tailrec
  private def commandLoop(board: Board): PlayerAction = {

    val Loc = """(\d)\s*(\d)""".r

    Console.readLine(s"$board\nYour move, please?\n> ") match {
      case "q" | "quit" =>
        Forfeit

      case Loc(c, r) =>
        val location = Location(c.toInt, r.toInt)
        if (board.isLegalMoveDestination(location)) {
          Claim(Occupy(location))
        } else {
          println("Illegal move!")
          commandLoop(board)
        }

      case x =>
        println(s"Not a move: [$x]")
        commandLoop(board)

    }
  }


}
