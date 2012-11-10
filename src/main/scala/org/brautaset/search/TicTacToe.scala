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

  val d = Vector((0 to 2).map(x => Point(x, x)), (0 to 2).map(x => Point(2-x, x)))
  val h = for (x <- 0 to 2) yield (0 to 2).map(y => Point(x, y))
  val v = for (x <- 0 to 2) yield (0 to 2).map(y => Point(y, x))
  val rows = d ++ h ++ v

  def fitness = {
    def score(p: Piece) =
      if (p.isFree) 0
      else if (p == current) 1
      else -1

    // Vector of all 8 rows and their pieces
    val f = rows.map(_.map(p => grid(p)))

    // Ignore any row that contains both player and opponent pieces
    val f1 = f.filterNot(r => r.contains(current) && r.contains(current.opponent))

    // Map player to +1 and opponent to -1 and sum their score in each row
    val g = f1.map(_.map(p => score(p)).sum)

    // Discard rows with no score
    val h = g.filterNot(_ == 0)

    // partition player / opponent scores
    val (i, j) = h.partition(_ > 0)

    // Calculate individual player scores
    val p = i.map(math.pow(10, _)).sum.toInt
    val o = j.map(x => math.pow(10, -x)).sum.toInt

    // Calculate overall fitness
    p - o
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