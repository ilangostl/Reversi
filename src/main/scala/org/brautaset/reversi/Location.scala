package org.brautaset.reversi

case class Location(column: Int, row: Int) {
  import Location._

  def moveBy(dx: Int, dy: Int) = copy(column + dx, row + dy)

  lazy val isOnBoard =
    column >= 0 && column < columns && row >= 0 && row < rows

  def neighbours: Set[Location] = {
    val neighbours = for {
      c <- -1 to 1
      r <- -1 to 1
    } yield moveBy(c, r)
    neighbours.filter(_.isOnBoard).toSet - this
  }

}

object Location {
  val columns = 8
  val rows = 8

  def apply(column: Char, row: Int) = new Location(column - 'a', row)

  def neighbours(locations: Set[Location]) =
    locations.flatMap(_.neighbours) -- locations
}