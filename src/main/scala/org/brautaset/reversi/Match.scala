package org.brautaset.reversi

import akka.actor.{ActorLogging, ActorRef, Actor}

object Match {

  case class Start(reportTo: ActorRef)
  case object Continue

  case class ProgressReport(board: Board)
  case class GameOver(board: Board)

  case class TakeTurn(board: Board)
  case class MakeMove(location: Location)

}

class Match(p1: ActorRef, p2: ActorRef) extends Actor with ActorLogging {

  import Match._

  var moves = List.empty[Location]
  var board = Board()

  val turn = Map(
    board.turn -> p1,
    board.turn.opponent -> p2)

  var reportTo: Option[ActorRef] = None

  def receive = {
    case Start(starter) =>
      reportTo = Some(starter)
      self ! Continue

    case Continue =>
      if (board.isFinished)
        reportTo.get ! GameOver(board)
      else {
        reportTo.get ! ProgressReport(board)
        turn(board.turn) ! TakeTurn(board)
      }

    case MakeMove(location: Location) =>
      log.debug(s"Got move: $location")

      moves ::= location
      board = board.successor(location)

      self ! Continue
  }

}
