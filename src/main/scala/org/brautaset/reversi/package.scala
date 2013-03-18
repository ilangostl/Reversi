package org.brautaset

package object reversi {

  val columns = 8
  val rows = 8

  case class Location(column: Int, row: Int) {
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
    def apply(column: Char, row: Int) = new Location(column - 'a', row)
  }

  sealed trait Player
  case object X extends Player
  case object O extends Player

  object Player {
    val first = X
    def opponent(player: Player) = if (player == X) O else X
  }

  type Board = Map[Location,Player]

}
