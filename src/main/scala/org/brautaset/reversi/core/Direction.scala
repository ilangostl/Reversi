package org.brautaset.reversi.core

case class Direction(x: Int, y: Int)

object Direction {

  private val dirs = for {
    x <- -1 to 1
    y <- -1 to 1
  } yield Direction(x, y)

  val all = dirs.filterNot(d => d.x == 0 && d.y == 0).toSet

}