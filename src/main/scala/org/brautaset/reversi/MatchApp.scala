package org.brautaset.reversi

import akka.actor.ActorDSL._
import akka.actor.{ActorLogging, Props, ActorSystem}

object MatchApp extends App {

  import Match._

  implicit val system = ActorSystem("match-system")

  val List(p1, p2) = List("Foo", "Bar").map(system.actorOf(Props[Player], _))

  actor(new Act with ActorLogging {

    val Match = system.actorOf(Props(new Match(p1, p2, self)))
    Match ! Go

    become {
      case ProgressReport(board) =>
        log.info(s"Current state:\n$board")
        Match ! Go

      case GameOver(board) =>
        log.info(s"Game Over:\n$board")
        system.shutdown()
    }
  })


}
