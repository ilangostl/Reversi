package org.brautaset.reversi

object Reversi {

  val initialBoard = Map(
    Location('d', 4) -> X,
    Location('d', 5) -> O,
    Location('e', 4) -> O,
    Location('e', 5) -> X)

  def apply(initialPlayer: Player, initialBoard: Board) =
    new Reversi(initialPlayer, initialBoard)

  def apply(initialPlayer: Player): Reversi =
    Reversi(initialPlayer, initialBoard)

  def apply(initialBoard: Board): Reversi =
    Reversi(Player.first, initialBoard)

  def apply(): Reversi =
    Reversi(Player.first, initialBoard)

}

class Reversi(initialPlayer: Player, initialBoard: Board) {

  import Reversi._

  var currentPlayer = initialPlayer
  var currentBoard = initialBoard


}
