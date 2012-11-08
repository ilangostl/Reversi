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

case class Slot(x: Int, y: Int) extends Move

class TicTacToe(grid: Vector[Vector[Piece]], current: Piece) extends State {

  def legalMoves = {
    for {
      x <- 0 to grid.length
      y <- 0 to grid(0).length
      if (grid(x)(y).isFree)
    } yield Slot(x, y)
  }.toList

  def successor(move: Move) = move match {
    case Slot(x, y) => {
      val newGrid = grid.updated(x, grid(x).updated(y, current))
      new TicTacToe(newGrid, current.opponent)
    }
    case _ => throw new Exception("Expected a Slot!")
  }

  def fitness = 0

  override def toString = grid.map(_.mkString("")).mkString("\n")

}

object TicTacToe {

  def apply: TicTacToe = this(Cross)

  def apply(slot: Piece): TicTacToe = new TicTacToe(Vector.fill(3, 3)(Empty), slot)

}