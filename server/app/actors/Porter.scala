package actors


import akka.actor._
import scala.collection.immutable.ListMap

import shared.{BoardSize,Game,Move}
import score.Result


object PorterActor {
  case class NewGame(boardSize: BoardSize)
  case class NewMove(move: Move)
  case object StartEngine
  case object NewSocket
  case class BestMove(coord: String)
  case class BestMoves(bestMoves: ListMap[Move, Result])
}

class PorterActor() extends Actor with ActorLogging {
  var gameOpt: Option[Game] = None // maybe store more games in the future
  var engineActorOpt: Option[ActorRef] = None
  var socketRefOpt: Option[ActorRef] = None

  def receive = {
    case PorterActor.NewGame(boardSize) => {
      gameOpt = Some(Game(boardSize))
      log.info("Created new game")
    }
    case PorterActor.NewMove(move) => {
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
    case PorterActor.StartEngine => {
      // create EngineActor if it does not exist
      log.info("starting thinking")

      engineActorOpt match {
        case Some(_) => { }
        case None => { engineActorOpt = Some(context.actorOf(Props[EngineActor], name="firstEngine")) }
      }

      for {
        engineActor <- engineActorOpt
        game <- gameOpt
      } yield { engineActor ! EngineActor.Start(game) }
    }
    case PorterActor.NewSocket => {
      log.info("setup new socket")
      socketRefOpt = Some(sender)
    }
    case PorterActor.BestMove(coord) => {
      // log.info("learn about the best move currently")
      // tell websocket something
      socketRefOpt match {
        case Some(socket) => socket ! WebSocketActor.BestMove(coord)
        case None => log.info("websocket not available")
      }
    }
    case PorterActor.BestMoves(bestMoves) => {
      log.info("Porter forwards best moves")
      socketRefOpt match {
        case Some(socket) => {
          log.info("telling WebSocket something")
          socket forward WebSocketActor.BestMoves(bestMoves)
        }
        case None => log.info("websocket not available")
      }
    }
    case _ => {
      log.info("huh?")
      log.info("Porter: unknown message %s".format(sender.path.toString))
    }
  }
}
