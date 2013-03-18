package org.brautaset

package object reversi {

  val nOfColumns = 8
  val nOfRows = 8

  case class Location(column: Int, row: Int) {
    def moveBy(dx: Int, dy: Int) = copy(column + dx, row + dy)
  }

  object Location {
    def apply(column: Char, row: Int) = new Location(column - 'a', row)
    def isOnBoard(location: Location) =
      location.column >= 0 &&
        location.column < nOfColumns &&
        location.row >= 0 &&
        location.row < nOfRows
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
