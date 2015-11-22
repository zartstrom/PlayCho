package actors


import akka.actor._

import shared.{Game,Move}


object PorterActor {
  case object NewSocket
  case class CurrentBestMove(coord: String)
}

class PorterActor() extends Actor with ActorLogging {
  var gameOpt: Option[Game] = None // maybe store more games in the future
  var engineActorOpt: Option[ActorRef] = None
  var socketRefOpt: Option[ActorRef] = None

  def receive = {
    case messages.NewGame(boardSize) => {
      gameOpt = Some(Game(boardSize))
      log.info("Created new game")
    }
    case messages.NewMove(move) => {
      log.info("Player %d played at %s".format(move.player, move.coord))

      gameOpt match {
        case Some(game) => {
          game.make(move)(game.board.position) // ugly, make game.board.position implicit?! 
        }
        case None => {
          log.error("No game available")
        } // do something about it
      }
    }
    case messages.StartThinking => {
      // create EngineActor if it does not exist
      log.info("starting thinking")

      engineActorOpt match {
        case Some(_) => { }
        case None => { engineActorOpt = Some(context.actorOf(Props[EngineActor], name="firstEngine")) }
      }

      for {
        engineActor <- engineActorOpt
        game <- gameOpt
      } yield { engineActor ! messages.StartEngine(game) }
    }
    case PorterActor.NewSocket => {
      log.info("setup new socket")
      socketRefOpt = Some(sender)
    }
    case PorterActor.CurrentBestMove(coord) => {
      log.info("learn about the best move currently")
      // tell websocket something
      socketRefOpt match {
        case Some(socket) => socket forward PorterActor.CurrentBestMove(coord)
        case None => log.info("websocket not available")
      }
    }
    case _ => {
      log.info("huh?")
      log.info("Porter: unknown message %s".format(sender.path.toString))
    }
  }
}
