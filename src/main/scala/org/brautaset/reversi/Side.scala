package org.brautaset.reversi

abstract sealed trait Side {
  def opponent = Side.opponent(this)
}

case object X extends Side
case object O extends Side

object Side {
  protected def opponent(player: Side) = if (player == X) O else X
}
