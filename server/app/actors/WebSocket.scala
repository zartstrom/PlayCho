package actors


import akka.actor._
import play.api.Logger
import play.api.libs.json._
import scala.collection.immutable.ListMap

import score.Result
import shared.Move


object WebSocketActor {
  def props(manager: ActorRef, out: ActorRef) = {
    Props(new WebSocketActor(manager, out))
  }

  case class BestMove(coord: String)
  case class BestMoves(bestMoves: ListMap[Move, Result])
}


class WebSocketActor(manager: ActorRef, out: ActorRef) extends Actor with ActorLogging {
  override def preStart(): Unit = {
    Logger.info("WebSocketActor: register myself at %s".format(manager.path.toString))
    manager ! PorterActor.NewSocket
  }

  def receive = {
    case msg: String => {
      Logger.info("Logger: websocket actor says hi")
      out ! ("We received your message, hohohohoho: " + msg)
    }
    case WebSocketActor.BestMove(coord) => {
      out ! "Current best move: %s".format(coord)
    }
    case WebSocketActor.BestMoves(bestMoves) => {
      Logger.info("WebSocket forwards best moves")
      // toJson
      def f(ab: (Move, Result)): JsValue = {
        ab match {
          case (move, result) => {
            Json.toJson( Map(
              "coord" -> move.coord.toString,
              "value" -> "%.2f".format(result.average),
              "nofGames" -> result.nofGames.toString
            ) )
          }
        }
      }
      val seq = bestMoves map f
      val resultJson = Json.stringify(Json.toJson(Map("moves" -> seq)))
      Logger.info(resultJson)
      out ! resultJson
    }
    case _ => {
      log.info("Huhh?")
      out ! "Huhh?"
    }
  }
}
