package org.brautaset.reversi

import akka.actor.ActorDSL._
import akka.actor.{ActorRef, ActorLogging, Props, ActorSystem}

object MatchApp extends App {

  import Match._

  implicit val system = ActorSystem("match-system")

  val List(p1, p2) = List("Foo", "Bar").map(system.actorOf(Props[Player], _))

  actor(new Act with ActorLogging {

    val Match = context.actorOf(Props(new Match(self)))
    Match ! Start(p1, p2)

    become {

      case Ongoing(board, turn: ActorRef) =>
        log.info(s"Turn: ${turn.path.name}")
        Match ! Go

      case Finished(board, winner) =>
        log.info(s"Game Over:\n$board")

        if (winner.isDefined)
          log.info(s"It was won by ${winner.get.path.name}")
        else
          log.info("It ended in a draw")

        system.shutdown()
    }
  })


}
