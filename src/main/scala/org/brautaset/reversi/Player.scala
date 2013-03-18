package org.brautaset.reversi

sealed trait Player
case object X extends Player
case object O extends Player

object Player {
  def opponent(player: Player) = if (player == X) O else X
}
