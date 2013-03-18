package org.brautaset.reversi

object Board {

  val columns = 8
  val rows = 8

  val initialGrid = Map(
    Location(4, 4) -> X,
    Location(4, 5) -> O,
    Location(5, 4) -> O,
    Location(5, 5) -> X)

  def apply(): Board =
    Board(X, initialGrid)

  def isOnBoard(location: Location) =
    location.column >= 0 && location.column < columns && location.row >= 0 && location.row < rows

}

case class Board(playerTurn: Player, grid: Map[Location,Player]) {

  import Board._

  def neighboursOfOccupiedLocations = {
    val occupiedLocations = grid.keySet
    (occupiedLocations.flatMap(_.neighbours) -- occupiedLocations).filter(isOnBoard(_))
  }

}
