package global


import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import play.api._


object Global extends GlobalSettings {

  val mySystem = ActorSystem("moves")
  val receptionist = mySystem.actorOf(Props(new engine.MoveReceiveActor()), name = "Dave")

}
