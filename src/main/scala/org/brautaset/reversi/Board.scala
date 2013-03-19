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

    def inner(d: Direction): Boolean = {

      @tailrec
      def inner0(l: Location): Boolean =
       if (locationsHeldByOpponent.contains(l))
          inner0(l.moveBy(d))
        else
          occupiedlocations.contains(l)

      // we must skip over at least one of opponent's pieces to be legal move
      val next = location.moveBy(d)
      if (locationsHeldByOpponent.contains(next))
        inner0(next.moveBy(d))
      else
        false
    }

    @tailrec
    def outer(dirs: List[Direction]): Boolean =
      if (dirs.isEmpty)
        false
      else if (inner(dirs.head))
        true
      else
        outer(dirs.tail)

    outer(Location.directions.toList)
  }

  lazy val legalMoves =
    unoccupiedNeighboursToOpponent.filter(isLegalMove(_))

}
