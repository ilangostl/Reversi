package org.brautaset.search

/**
 * Created with IntelliJ IDEA.
 * User: SuperPappi
 * Date: 08/11/2012
 * Time: 08:15
 * To change this template use File | Settings | File Templates.
 */
/*
class NegaMax extends Search[M <: Move, S <: State] {

  private def negamax(state: S, ply: Int): Int =
    if (ply <= 0)
      state.fitness
    else
      state.legalMoves.map( m => -negamax(state.successor(m), ply - 1)).max

  def search(state: S, ply: Int) = {
    val groups = state.legalMoves.groupBy(m => negamax(state.successor(m), ply - 1))
    val max = groups.keys.max
    println(groups + " => " + max)
    groups(max).head
  }

}
*/