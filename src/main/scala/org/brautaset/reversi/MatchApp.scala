package org.brautaset.reversi

import akka.actor.ActorDSL._
import akka.actor.{ActorLogging, Props, ActorSystem}

object MatchApp extends App {

  implicit val system = ActorSystem("match-system")

  val List(p1, p2) = List("Foo", "Bar").map(system.actorOf(Props[Player], _))

  val thematch = system.actorOf(Props(new Match(p1, p2)))

  actor(new Act with ActorLogging {
    import Match._
    thematch ! Start(self)
    become {
      case ProgressReport(board) =>
        log.info(s"Current state:\n$board")

      case GameOver(board) =>
        log.info(s"Game Over:\n$board")
        system.shutdown()
    }
  })


}
