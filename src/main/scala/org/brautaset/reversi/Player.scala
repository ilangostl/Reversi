package org.brautaset.reversi

import akka.actor.Actor

class Player extends Actor {

  import Match._

  def receive = {
    case Move(board) =>
      sender ! board.legalMoves.head
  }

}
