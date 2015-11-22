package controllers


import akka.actor.Props
import play.api.mvc._
import play.api.routing.JavaScriptReverseRouter

import global._

import shared.{Board, BoardSize, Coord, Move}


object Application extends Controller {

  def index = Action {
    Ok(views.html.board())
  }

  def game = Action(parse.json) { request =>

    // what about deserializing a BoardSize object?!
    val boardSizeOpt = for {
      x <- (request.body \ "sizeX").asOpt[Int]
      y <- (request.body \ "sizeY").asOpt[Int]
    } yield BoardSize(x, y)

    boardSizeOpt match {
      case Some(b) => {
        Global.porter ! messages.NewGame(b)
        Ok("create Board")
      }
      case None => BadRequest(<p>Missing value(s)</p>)
    }
  }

  def move = Action(parse.json) { request =>

    // what about deserializing a Move object?!
    val moveOpt = for {
      c <- (request.body \ "coord").asOpt[String]
      p <- (request.body \ "player").asOpt[Int]
      x <- (request.body \ "sizeX").asOpt[Int]
      y <- (request.body \ "sizeY").asOpt[Int]
    } yield { messages.NewMove(Move(Coord(c)(BoardSize(x, y)), p)) }  // think about that boardSize stuff

    moveOpt match {
      case Some(m) => {
        Global.porter ! m
        Ok("it wooooorrrrrked!!!!")
      }
      case None => BadRequest(<p>Missing value(s)</p>)
    }
  }

  // javascript routes
  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes")(
        routes.javascript.Application.think
      )
    ).as("text/javascript")
  }

  // used when clicking button in board.scala.html
  def think = Action { request =>
    println("dflajdlfajlf")
    Global.porter ! messages.StartThinking
    Ok("told Engine to start thinking")
  }
}
