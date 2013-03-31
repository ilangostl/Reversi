package org.brautaset.reversi

import annotation.tailrec
import org.brautaset.reversi.Location.Direction

object Board {

  val columns = 8
  val rows = 8

  val initialGrid = Map(
    Location(3, 3) -> O,
    Location(3, 4) -> X,
    Location(4, 3) -> X,
    Location(4, 4) -> O)

  def apply(): Board =
    Board(X, initialGrid)

  def isOnBoard(location: Location) =
    location.column >= 0 && location.column < columns && location.row >= 0 && location.row < rows

}

case class Board(turn: Piece, grid: Map[Location,Piece]) {

  import Board._

  def occupiedlocations = grid.keySet

  private lazy val locationsHeldByOpponent =
    grid.filterNot(_._2 == turn).keySet

  private def unoccupiedNeighboursToLocationsHeldByOpponent =
    locationsHeldByOpponent.flatMap(_.neighbours).filter(isOnBoard(_)) -- occupiedlocations

  private def flippedLocations(loc: Location, d: Direction) = {
    @tailrec
    def iter(l: Location, flipped: List[Location]): List[Location] =
      if (locationsHeldByOpponent.contains(l))
        iter(l.moveBy(d), l :: flipped)
      else if (occupiedlocations.contains(l))
        flipped
      else
        Nil

    // we must skip over at least one of opponent's pieces to be legal move
    val next = loc.moveBy(d)
    if (locationsHeldByOpponent.contains(next))
      iter(next.moveBy(d), next :: Nil)
    else
      Nil
  }

  def isLegalMove(location: Location) =
    Location.directions.find(!flippedLocations(location, _).isEmpty).isDefined

  def legalMoves =
    unoccupiedNeighboursToLocationsHeldByOpponent.filter(isLegalMove(_))

  private def locationsFlippedByMove(location: Location) =
    Location.directions.flatMap(flippedLocations(location, _))

  def successor(move: Location) =
    if (isLegalMove(move))
      Board(turn.opponent, grid ++ (locationsFlippedByMove(move) + move).map((_, turn)))
    else
      throw new IllegalArgumentException(s"$move is not a legal move at:" + this)

  def isFinished = legalMoves.isEmpty

  def finishScore = {
    winner match {
      case None => 0
      case Some(`turn`) => Int.MaxValue
      case _ => -Int.MaxValue
    }
  }

  def winner = {
    val diff = grid.values.collect {
      case X => 1
      case O => -1
    }.sum

    if (diff == 0)
      None
    else
      Some(if (diff > 0) X else O)
  }

  override def toString = {
    def line(r: Int) =
      r +: (0 until columns).map {
        c => grid.get(Location(c, r))
      }.collect {
        case None => "."
        case Some(p) => p.toString
      }

    val head = (0 until columns).mkString("\n/", "", "")
    val body = (0 until rows).map(line(_).mkString)
    val foot = if (isFinished) winner match {
      case Some(w) => w + " won"
      case None => "Draw"
    } else turn.toString + " to move"
    (head +: body :+ foot).mkString("\n")
  }

}
