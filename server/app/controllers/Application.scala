package controllers


import akka.actor.Props
import play.api.libs.iteratee.{Concurrent,Iteratee}
import play.api.libs.json._
import play.api.Logger
import play.api.mvc._
import play.api.Play.current
import play.api.routing.JavaScriptReverseRouter
// do not use ExecutionContext like this. See
// https://github.com/alexandru/scala-best-practices/blob/master/sections/4-concurrency-parallelism.md#411-must-not-hardcode-the-thread-pool--execution-context
import scala.concurrent.ExecutionContext.Implicits.global

import main._
import shared.{Board, BoardSize, Coord, Move}
import actors.Msg


object Application extends Controller {

  def index = Action {
    Ok(views.html.board())
  }

  def game = Action(parse.json) { request =>

    // what about deserializing a BoardSize object?!
    val boardSizeResult: JsResult[BoardSize] = for {
      x <- (request.body \ "x").validate[Int]
      y <- (request.body \ "y").validate[Int]
    } yield BoardSize(x, y)

    boardSizeResult match {
      case JsSuccess(b, _) => {
        Global.porter ! Msg.NewGame(b)
        Ok("created game")
      }
      case e: JsError => {
        Logger.error("Could not create game: " + JsError.toJson(e).toString)
        BadRequest(JsError.toJson(e))
      }
    }
  }

  def move = Action(parse.json) { request =>

    // what about deserializing a Move object?!
    val moveRes: JsResult[Move] = for {
      c <- (request.body \ "coord" \ "string").validate[String]
      x <- (request.body \ "coord" \ "x").validate[Int]
      y <- (request.body \ "coord" \ "y").validate[Int]
      p <- (request.body \ "player").validate[Int]
    } yield { Move(Coord(c)(BoardSize(x, y)), p) }  // think about that boardSize stuff

    moveRes match {
      case JsSuccess(move, _) => {
        Global.porter ! Msg.NewMove(move)
        Ok("received move")
      }
      case e: JsError => {
        Logger.error("Could not create move: " + JsError.toJson(e).toString)
        BadRequest(JsError.toJson(e))
      }
    }
  }

  // javascript routes
  def javascriptRoutes = Action { implicit request =>
    Ok(
      JavaScriptReverseRouter("jsRoutes")(
        routes.javascript.Application.think
        //routes.javascript.Application.indexWS
      )
    ).as("text/javascript")
  }

  // used when clicking button in board.scala.html
  def think = Action { request =>
    Logger.info("let me think about it")
    Global.porter ! Msg.StartThinking
    Ok("told Engine to start thinking")
  }

  def socket = WebSocket.acceptWithActor[String, String] { request => out =>
    Props(new actors.MyWebSocketActor(Global.porter, out))
  }
}
