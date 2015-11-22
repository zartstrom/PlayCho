package actors


import akka.actor._
import scala.concurrent.duration._
import scala.util.Random

import shared.{Board,Coord,Game,Move}

import main.Global.porter
import messages.Simple
import PorterActor.{CurrentBestMove}


object EngineActor {
  case class GiveBestMove(game: Game)
}


class EngineActor() extends Actor with ActorLogging {
  import context._
  var loop = 0

  def receive = {
    case messages.StartEngine(game: Game) => {
      log.info("starting engine")
      // send another periodic tick after the specified delay
      loop = 0
      system.scheduler.scheduleOnce(1000 millis, self, EngineActor.GiveBestMove(game))
    }
    case EngineActor.GiveBestMove(game) => {
      val coord: Coord = game.randomMove(Board.BLACK)
      log.info("give best move %s".format(coord.toString))

      porter ! CurrentBestMove(coord.toString)
      // loop
      loop += 1
      if (loop < 3) { system.scheduler.scheduleOnce(1000 millis, self, EngineActor.GiveBestMove(game)) }
    }
  }
}


