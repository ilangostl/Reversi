package org.brautaset.reversi

import akka.actor.{ActorLogging, ActorRef, Actor}

object Match {

  // Make with the going-on.
  case object Go

  // What's the current status?
  case object Check

  // Messages returned by Check
  case class Prestart(board: Board)
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
      context.become(gameOn(initialBoard, p1, p2))
      self forward Go

    case Check =>
      sender ! Prestart(initialBoard)
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
