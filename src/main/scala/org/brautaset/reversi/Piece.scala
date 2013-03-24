package org.brautaset.reversi

abstract sealed trait Piece {
  def opponent = Piece.opponent(this)
}

case object X extends Piece
case object O extends Piece

object Piece {
  protected def opponent(player: Piece) = if (player == X) O else X
}
