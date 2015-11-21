package controllers


import akka.actor.Props
import play.api.mvc._

import global._

import shared.{Board, BoardSize, Coord, Move}


object Application extends Controller {

  def index = Action {
    Ok(views.html.board())
  }

  //def testAkka(something: String) = Action(BodyParsers.parse.json) { request =>
  def testAkka(something: String) = Action { request =>

    val helloActor = Global.mySystem.actorOf(Props(new engine.HelloActor("Fred")), name = "helloactor")
    helloActor ! engine.SimpleMessage(something)

    Ok("told him")
  }

  def move = Action(parse.json) { request =>

    implicit val boardSize = BoardSize(9, 9) // need to get it from Client

    val moveOpt = for {
      c <- (request.body \ "coord").asOpt[String]
      p <- (request.body \ "player").asOpt[Int]
    } yield { Move(Coord(c), p) }
    
    moveOpt match {
      case Some(m) => {
        Global.receptionist ! m
        Ok("it wooooorrrrrked!!!!")
      }
      case None => Ok("not Ok")  // TODO: 404 or something
    }
  }
}
