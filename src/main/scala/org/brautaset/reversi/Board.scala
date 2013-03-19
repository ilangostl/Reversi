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

  def occupiedlocations = grid.keySet

  lazy val locationsHeldByOpponent =
    grid.filterNot(_._2 == playerTurn).keySet

  def unoccupiedNeighboursToLocationsHeldByOpponent =
    locationsHeldByOpponent.flatMap(_.neighbours).filter(isOnBoard(_)) -- occupiedlocations

  def isLegalMove(location: Location) =
    !flippedLocations(location).isEmpty

  def legalMoves =
    unoccupiedNeighboursToLocationsHeldByOpponent.filter(isLegalMove(_))

  def flippedLocations(location: Location) = {
    def flippedLocationDirection(d: Direction) = {

      @tailrec
      def iter(l: Location, flipped: List[Location]): List[Location] =
        if (locationsHeldByOpponent.contains(l))
          iter(l.moveBy(d), l :: flipped)
        else if (occupiedlocations.contains(l))
          flipped
        else
          Nil

      // we must skip over at least one of opponent's pieces to be legal move
      val next = location.moveBy(d)
      if (locationsHeldByOpponent.contains(next))
        iter(next.moveBy(d), next :: Nil)
      else
        Nil
    }

    Location.directions.flatMap(flippedLocationDirection(_))
  }

  def successor(move: Location) = {
    val gd = (flippedLocations(move) + move).map(x => (x, playerTurn)).toMap
    Board(playerTurn.opponent, grid ++ gd)
  }

  override def toString = {
    def line(r: Int) =
      r +: (0 until columns).map {
        c => grid.get(Location(c, r))
      }.collect {
        case None => "."
        case Some(p) => p.toString
      }

    val head = "/" + (0 until columns).mkString
    val body = (0 until rows).map(line(_).mkString)
    val foot = playerTurn.toString + " to move"
    (head +: body :+ foot).mkString("\n")
  }

}
