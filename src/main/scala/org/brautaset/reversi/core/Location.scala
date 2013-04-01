package org.brautaset.reversi.core

case class Location(column: Int, row: Int) {

  def moveBy(d: Direction) =
    copy(column + d.x, row + d.y)

  def neighbours: Set[Location] =
    Direction.all.map(moveBy(_))

}

