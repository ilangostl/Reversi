package org.brautaset

/**
 * Created with IntelliJ IDEA.
 * User: SuperPappi
 * Date: 08/11/2012
 * Time: 08:14
 * To change this template use File | Settings | File Templates.
 */
package object search {

  trait Move

  trait State[M <: Move] {

    def moves: Set[M]

    def successor(move: M): State[M]

    def fitness: Int

    lazy val isLeaf = moves.isEmpty

  }

  trait Search {

    type M <: Move

    def search(state: State[M], ply: Int): M

  }

}
