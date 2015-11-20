package engine


import akka.actor._

import shared.Move


case class SimpleMessage(text: String)


class HelloActor(name: String) extends Actor with ActorLogging{
  def receive = {
    case "hello" => log.info("hello received")
    case SimpleMessage(txt) => log.info("message received: %s".format(txt))
    case _ => log.info("catch call received")
  }
}

class MoveReceiveActor() extends Actor with ActorLogging {
  def receive = {
    case Move(coord, player) => log.info("Player %d played at %s".format(player, coord))
    case _ => log.info("huh?")
  }
}

object Engine {

}


class Engine(position: Array[Int]) {

}
