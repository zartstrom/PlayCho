package main


import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import play.api._
import play.api.mvc._
import play.api.Play.current


object Global extends GlobalSettings {

  val mySystem = ActorSystem("moves")
  val porter = mySystem.actorOf(Props(new actors.PorterActor()), name = "singletonGame")

}
