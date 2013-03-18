package org.brautaset.reversi

object Board {

  val firstPlayer = X

  val initialLocations = Map(
    Location('d', 4) -> X,
    Location('d', 5) -> O,
    Location('e', 4) -> O,
    Location('e', 5) -> X)

  def apply(playerTurn: Player = X, board: Map[Location,Player] = initialLocations) =
    new Board(playerTurn, board)

}

class Board(initialPlayer: Player, initialBoard: Map[Location,Player]) {

  import Board._

  var currentPlayer = initialPlayer
  var currentBoard = initialBoard


}
