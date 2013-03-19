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

  lazy val unoccupiedNeighboursToOpponent =
    locationsHeldByOpponent.flatMap(_.neighbours).filter(isOnBoard(_)) -- occupiedlocations

  def isLegalMove(location: Location): Boolean = {

    @tailrec
    def inner(l: Location, d: Direction): Boolean =
      if (locationsHeldByOpponent.contains(l))
        inner(l.moveBy(d), d)
      else
        occupiedlocations.contains(l)

    @tailrec
    def outer(loc: Location, d: List[Direction]): Boolean =
      if (d.isEmpty)
        false
      else {
        val next = loc.moveBy(d.head)
        if (locationsHeldByOpponent.contains(next) && inner(next, d.head))
          true
        else
          outer(loc, d.tail)
      }

    outer(location, Location.directions.toList)
  }

  lazy val legalMoves =
    unoccupiedNeighboursToOpponent.filter(isLegalMove(_))

}
