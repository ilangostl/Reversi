package org.brautaset.reversi

case class Location(column: Int, row: Int) {
  import Location._

  def moveBy(d: Direction) =
    copy(column + d.x, row + d.y)

  def neighbours: Set[Location] =
    directions.map(moveBy(_))

}

object Location {

  case class Direction(x: Int, y: Int)

  private val dirs = for {
    x <- -1 to 1
    y <- -1 to 1
  } yield Direction(x, y)

  val directions = dirs.filterNot(d => d.x == 0 && d.y == 0).toSet

}