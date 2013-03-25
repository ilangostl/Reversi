package org.brautaset.reversi

import akka.actor.Actor

class Player extends Actor {

  import Match._

  def receive = {
    case YourTurn(board) =>
      sender ! Put(board.legalMoves.head, self)
  }

}
