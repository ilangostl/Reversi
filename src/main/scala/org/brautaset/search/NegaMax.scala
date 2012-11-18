package org.brautaset.search

/**
 * Created with IntelliJ IDEA.
 * User: SuperPappi
 * Date: 08/11/2012
 * Time: 08:15
 * To change this template use File | Settings | File Templates.
 */
trait NegaMax extends Search {

  private def negamax(state: State[M], ply: Int): Int =
    if (ply <= 0)
      state.fitness
    else
      state.moves.map( m => -negamax(state.successor(m), ply - 1)).max

  override def search(state: State[M], ply: Int): M = {
    require(ply > 0)
    val groups = state.moves.groupBy(m => negamax(state.successor(m), ply - 1))
    val max = groups.keys.head
    groups(max).head
  }

}
