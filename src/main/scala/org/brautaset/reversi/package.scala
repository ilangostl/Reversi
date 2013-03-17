package org.brautaset

package object reversi {

  type Location = (Char,Char)
  type Board = Map[Location,Player]

  sealed trait Player
  case object X extends Player
  case object O extends Player

  object Player {

    val first = X

    def opponent(player: Player) =
      if (player == X) O else X

  }
}
