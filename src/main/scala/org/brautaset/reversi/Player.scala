package org.brautaset.reversi

abstract sealed trait Player {
  def opponent = Player.opponent(this)
}

case object X extends Player
case object O extends Player

object Player {
  protected def opponent(player: Player) = if (player == X) O else X
}
