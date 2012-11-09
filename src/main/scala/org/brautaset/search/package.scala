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

  trait State {

    def legalMoves: Set[Move]

    def successor(move: Move): State

    def fitness: Int

    lazy val isLeaf = legalMoves.isEmpty

  }

  trait Search {

    def search(state: State, ply: Int): List[Move]

  }

}
