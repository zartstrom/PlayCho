package actors


import akka.actor._
import play.api.Logger


object MyWebSocketActor {
  //def props(out: ActorRef, name: String) = Props(new MyWebSocketActor(out), name=name)

  def props(manager: ActorRef, out: ActorRef) = {
    Props(new MyWebSocketActor(manager, out))
  }
}


class MyWebSocketActor(manager: ActorRef, out: ActorRef) extends Actor with ActorLogging {
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
  }
}
