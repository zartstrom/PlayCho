package actors


import akka.actor._
import play.api.Logger
import play.api.libs.json._
import scala.collection.immutable.ListMap

import score.Result
import shared.Move


object WebSocketActor {
  //def props(out: ActorRef, name: String) = Props(new MyWebSocketActor(out), name=name)

  def props(manager: ActorRef, out: ActorRef) = {
    Props(new WebSocketActor(manager, out))
  }

  case class BestMoves()

  case class ForwardBestMoves(bestMoves: ListMap[Move, Result])

}


class WebSocketActor(manager: ActorRef, out: ActorRef) extends Actor with ActorLogging {
  override def preStart(): Unit = {
    Logger.info("MyWebSocketActor: register myself at %s".format(manager.path.toString))
    manager ! Msg.NewSocket
  }

  def receive = {
    case msg: String => {
      Logger.info("Logger: websocket actor says hi")
      out ! ("We received your message, hohohohoho: " + msg)
    }
    case Msg.CurrentBestMove(coord) => {
      out ! "Current best move: %s".format(coord)
    }
    case WebSocketActor.ForwardBestMoves(bestMoves) => {
      log.info("WebSocket forwards best moves")
      // toJson
      def f(ab: (Move, Result)): JsValue = {
        ab match {
          case (move, result) => {
            Json.toJson( Map("coord" -> move.coord.toString, "value" -> "%.2f".format(result.average)) )
          }
        }
      }
      val seq = Seq(bestMoves map f)
      val jsonObject = Json.toJson(Map("moves" -> seq))
      out ! jsonObject
    }
    case _ => {
      log.info("Huhh?")
    }
  }
}
