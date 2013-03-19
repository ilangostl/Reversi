package org.brautaset.reversi

import annotation.tailrec
import org.brautaset.reversi.Location.Direction

object Board {

  val columns = 8
  val rows = 8

  val initialGrid = Map(
    Location(3, 3) -> X,
    Location(3, 4) -> O,
    Location(4, 3) -> O,
    Location(4, 4) -> X)

  def apply(): Board =
    Board(X, initialGrid)

  def isOnBoard(location: Location) =
    location.column >= 0 && location.column < columns && location.row >= 0 && location.row < rows

}

case class Board(playerTurn: Player, grid: Map[Location,Player]) {

  import Board._

  lazy val occupiedlocations = grid.keySet

  lazy val locationsHeldByOpponent =
    grid.filterNot(_._2 == playerTurn).keySet

  lazy val unoccupiedNeighboursToLocationsHeldByOpponent =
    locationsHeldByOpponent.flatMap(_.neighbours).filter(isOnBoard(_)) -- occupiedlocations

  def isLegalMove(location: Location) = {
    def isLegalMoveDirection(d: Direction) = {

      @tailrec
      def iter(l: Location): Boolean =
       if (locationsHeldByOpponent.contains(l))
          iter(l.moveBy(d))
        else
          occupiedlocations.contains(l)

      // we must skip over at least one of opponent's pieces to be legal move
      val next = location.moveBy(d)
      if (locationsHeldByOpponent.contains(next))
        iter(next.moveBy(d))
      else
        false
    }

    Location.directions.find(isLegalMoveDirection(_)).isDefined
  }

  lazy val legalMoves =
    unoccupiedNeighboursToLocationsHeldByOpponent.filter(isLegalMove(_))

}
