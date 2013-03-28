package org.brautaset.reversi

import akka.actor.{ActorLogging, ActorRef, Actor}

object Match {

  case class Start(p1: ActorRef, p2: ActorRef)

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

class Match(controller: ActorRef) extends Actor with ActorLogging {

  import Match._

  def receive = {
    case Start(player, opponent) =>
      context.become(gameOn(Board(), player, opponent, Map(X -> player, O -> opponent)))
      self.tell(Check, controller)
  }

  def gameOn(board: Board, player: ActorRef, opponent: ActorRef, map: Map[Piece,ActorRef]): Receive = {
    case Go =>
      player ! Move(board)

    case Check =>
      sender ! Ongoing(board, player)

    case Claim(move) if sender == player =>
      val newBoard = board.successor(move)
      context.become(
        if (newBoard.isFinished)
          gameOver(newBoard, newBoard.winner.map(map(_)))
        else
          gameOn(newBoard, opponent, player, map))
      self.tell(Check, controller)

  }

  def gameOver(board: Board, winner: Option[ActorRef]): Receive = {
    case Check =>
      sender ! Finished(board, winner)
  }


}
