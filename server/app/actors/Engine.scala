package actors


import scala.util.Random
import akka.actor._

import shared.{Game,Move}


class EngineActor() extends Actor with ActorLogging {

  def receive = {
    case messages.StartEngine(game: Game) => {
      log.info("starting engine")
      coord = game.makeRandomMove
    }
  }
}


