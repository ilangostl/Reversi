package org.brautaset.reversi.core

import annotation.tailrec

object Board {

  val columns = 8
  val rows = 8

  val initialCaptures: Map[Side, Set[Location]] = Map(
    O -> Set(Location(3, 3), Location(4, 4)),
    X -> Set(Location(3, 4), Location(4, 3)))

  def apply(): Board =
    new Board(X, initialCaptures)

  def isOnBoard(location: Location) =
    location.column >= 0 && location.column < columns && location.row >= 0 && location.row < rows

}

case class Board(turn: Side, captures: Map[Side, Set[Location]]) {
  // I keep leaving one or the other out and getting weird problems..
  require(captures contains X)
  require(captures contains O)

  import Board._

  private val occupied = captures.flatMap(_._2).toSet

  private val unoccupiedNeighbours = captures.map {
    case (k, v) => k -> (v.flatMap(_.neighbours).filter(isOnBoard(_)) -- occupied)
  }

  private def flippedLocations(side: Side, loc: Location, d: Direction) = {
    @tailrec
    def iter(l: Location, flipped: List[Location]): List[Location] =
      if (captures(side).contains(l))
        iter(l.moveBy(d), l :: flipped)
      else if (occupied.contains(l))
        flipped
      else
        Nil

    // we must skip over at least one of opponent's pieces to be legal move
    val next = loc.moveBy(d)
    if (captures(side).contains(next))
      iter(next.moveBy(d), next :: Nil)
    else
      Nil
  }

  def isLegalMove(location: Location): Boolean = isLegalMove(turn, location)
  def isLegalMove(side: Side, location: Location) =
    Direction.all.find(!flippedLocations(side.opponent, location, _).isEmpty).isDefined

  def legalMoves: Set[Location] = legalMoves(turn)
  def legalMoves(side: Side) =
    unoccupiedNeighbours(side.opponent).filter(isLegalMove(side, _))

  def successor(move: Location) = {
    require(isLegalMove(move))
    val flipped = Direction.all.flatMap(flippedLocations(turn.opponent, move, _)) + move
    Board(turn.opponent, Map(
      turn -> (captures(turn) ++ flipped),
      turn.opponent -> (captures(turn.opponent) -- flipped)
    ))
  }

  def isFinished = legalMoves.isEmpty

  def winner =
    (captures(X).size - captures(O).size) match {
      case 0 => None
      case x => Some(if (x > 0) X else O)
  }

  override def toString = {
    val grid = captures.flatMap { case (k, v) => v.map( _ -> k ) }
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
