package org.brautaset.reversi

import akka.actor.Actor

class Player extends Actor {

  import Match._

  def receive = {
    case TakeTurn(board) =>
      sender ! MakeMove(board.legalMoves.head, self)
  }

}
