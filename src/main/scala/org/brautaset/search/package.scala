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

  trait State[T <: Move] {

    def legalMoves: Set[T]

    def successor(move: T): State[T]

    def fitness: Int

    lazy val isLeaf = legalMoves.isEmpty

  }

  trait Search[M <: Move] {

    def search(state: State[M], ply: Int): M

  }

}
