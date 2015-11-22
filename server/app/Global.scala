package global


import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import play.api._


object Global extends GlobalSettings {

  val mySystem = ActorSystem("moves")
  val porter = mySystem.actorOf(Props(new actors.PorterActor()), name = "singletonGame")

}
