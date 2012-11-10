package org.brautaset.search

/**
 * Created with IntelliJ IDEA.
 * User: SuperPappi
 * Date: 08/11/2012
 * Time: 20:06
 * To change this template use File | Settings | File Templates.
 */

trait Piece {

  def isFree = false

  def opponent =
    if (this == Empty) throw new Exception("Expected to be called on Cross or Circle only")
    else if (this == Cross) Circle
    else Cross
}

object Cross extends Piece {
  override val toString = "x"
}

object Circle extends Piece {
  override val toString = "o"
}

object Empty extends Piece {
  override val toString = "-"

  override def isFree = true
}

case class Point(x: Int, y: Int) extends Move

class TicTacToe(grid: Map[Point, Piece], current: Piece) extends State[Point] {

  def legalMoves =
    grid.filter(_._2.isFree).keySet

  def successor(move: Point) =
    new TicTacToe(grid.updated(move, current), current.opponent)

  def fitness = {
    def fit(p: Piece) = {
      0
    }
    fit(current) - fit(current.opponent)
  }

  override def toString = {
    (0 to 2).map {
      x =>
        (for (y <- 0 to 2) yield grid(Point(x, y))) mkString
    } mkString "\n"
  }

}

object TicTacToe {

  def apply: TicTacToe = this(Cross)

  def apply(slot: Piece): TicTacToe = new TicTacToe({
    for {
      x <- 0 to 2
      y <- 0 to 2
    } yield (Point(x, y), Empty)
  }.toMap, slot)

}