package org.brautaset.reversi

import akka.actor.{ActorLogging, ActorRef, Actor}

object Match {

  // Make with the going-on.
  case object Go

  // What's the current status?
  case object Check

  // Messages returned by Check
  case class Ongoing(board: Board, turn: ActorRef)
  case class Finished(board: Board, winner: Option[ActorRef])

  // Message sent to players
  case class Move(board: Board)

  // Message returned from players
  case class Claim(location: Location)

}

class Match(p1: ActorRef, p2: ActorRef) extends Actor with ActorLogging {

  import Match._

  val initialBoard = Board()
  val piecePlayerMap = Map(
    initialBoard.turn -> p1,
    initialBoard.turn.opponent -> p2)

  def receive = {
    case Go =>
      p1 ! Move(initialBoard)

    case Check =>
      sender ! Ongoing(initialBoard, p1)

    case Claim(move) if sender == p1 =>
      context.become(gameOn(initialBoard.successor(move), p2, p1))
      self.tell(Check, context.parent)
  }

  def gameOn(board: Board, player: ActorRef, opponent: ActorRef): Receive = {
    case Go =>
      player ! Move(board)

    case Check =>
      sender ! Ongoing(board, player)

    case Claim(move) if sender == player =>
      val newBoard = board.successor(move)
      context.become(
        if (newBoard.isFinished)
          gameOver(newBoard, newBoard.winner.map(piecePlayerMap(_)))
        else
          gameOn(newBoard, opponent, player))
      self.tell(Check, context.parent)
  }

  def gameOver(board: Board, winner: Option[ActorRef]): Receive = {
    case Check =>
      sender ! Finished(board, winner)
  }


}
