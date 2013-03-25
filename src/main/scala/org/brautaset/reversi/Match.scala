package org.brautaset.reversi

import akka.actor.{ActorLogging, ActorRef, Actor}

object Match {

  case object Go
  case object CheckAndReport

  case class ProgressReport(board: Board)
  case class GameOver(board: Board)

  case class TakeTurn(board: Board)
  case class MakeMove(move: Location, player: ActorRef)

}

class Match(p1: ActorRef, p2: ActorRef, controller: ActorRef) extends Actor with ActorLogging {

  import Match._

  val initialBoard = Board()
  val piecePlayerMap = Map(
    initialBoard.turn -> p1,
    initialBoard.turn.opponent -> p2)

  def receive = {
    case Go =>
      p1 ! TakeTurn(initialBoard)

    case MakeMove(move, `p1`) =>
      context.become(gameOn(initialBoard.successor(move), p2, p1))
      self ! CheckAndReport
  }

  def gameOn(board: Board, player: ActorRef, opponent: ActorRef): Receive = {
    case Go =>
      player ! TakeTurn(board)

    case CheckAndReport =>
      if (board.isFinished) {
        context.become(gameOver(board))
        controller ! GameOver(board)
      } else
        controller ! ProgressReport(board)

    case MakeMove(move, `player`) =>
      context.become(gameOn(board.successor(move), opponent, player))
      self ! CheckAndReport
  }

  def gameOver(board: Board): Receive = {
    case _ => sender ! GameOver(board)
  }

}
