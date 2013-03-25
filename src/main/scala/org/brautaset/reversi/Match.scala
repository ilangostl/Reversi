package org.brautaset.reversi

import akka.actor.{ActorLogging, ActorRef, Actor}

object Match {

  case object Start
  case object Continue
  case object CheckAndReport

  case class ProgressReport(board: Board)
  case class GameOver(board: Board)

  case class TakeTurn(board: Board)
  case class MakeMove(location: Location)

}

class Match(p1: ActorRef, p2: ActorRef, controller: ActorRef) extends Actor with ActorLogging {

  import Match._

  val initialBoard = Board()

  val turn = Map(
    initialBoard.turn -> p1,
    initialBoard.turn.opponent -> p2)

  def receive = {
    case Start =>
      context.become(gameOn(initialBoard))
      self ! Continue
  }

  def gameOn(board: Board): Receive = {
    case Continue =>
      turn(board.turn) ! TakeTurn(board)

    case CheckAndReport =>
      if (board.isFinished) {
        context.become(gameOver(board))
        controller ! GameOver(board)

      } else
        controller ! ProgressReport(board)

    case MakeMove(location: Location) =>
      log.debug(s"Got move: $location")
      if (turn(board.turn) != sender)
        throw new IllegalStateException("Unexpected Sender")
      context.become(gameOn(board.successor(location)))
      self ! CheckAndReport
  }

  def gameOver(board: Board): Receive = {
    case Continue =>
      controller ! GameOver(board)
  }

}
