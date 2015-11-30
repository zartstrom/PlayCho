package actors


import akka.actor._

import shared.{Game,Move}


class PorterActor() extends Actor with ActorLogging {
  var gameOpt: Option[Game] = None // maybe store more games in the future
  var engineActorOpt: Option[ActorRef] = None
  var socketRefOpt: Option[ActorRef] = None

  def receive = {
    case Msg.NewGame(boardSize) => {
      gameOpt = Some(Game(boardSize))
      log.info("Created new game")
    }
    case Msg.NewMove(move) => {
      log.info("Player %d played at %s".format(move.player, move.coord))

      gameOpt match {
        case Some(game) => {
          game.make(move)
        }
        case None => {
          log.error("No game available")
        } // do something about it
      }
    }
    case Msg.StartThinking => {
      // create EngineActor if it does not exist
      log.info("starting thinking")

      engineActorOpt match {
        case Some(_) => { }
        case None => { engineActorOpt = Some(context.actorOf(Props[EngineActor], name="firstEngine")) }
      }

      for {
        engineActor <- engineActorOpt
        game <- gameOpt
      } yield { engineActor ! Msg.StartEngine(game) }
    }
    case Msg.NewSocket => {
      log.info("setup new socket")
      socketRefOpt = Some(sender)
    }
    case Msg.CurrentBestMove(coord) => {
      log.info("learn about the best move currently")
      // tell websocket something
      socketRefOpt match {
        case Some(socket) => socket forward Msg.CurrentBestMove(coord)
        case None => log.info("websocket not available")
      }
    }
    case _ => {
      log.info("huh?")
      log.info("Porter: unknown message %s".format(sender.path.toString))
    }
  }
}
