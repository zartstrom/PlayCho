package ui

import scala.util.{Try, Success, Failure}

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.ext.Ajax
import upickle.default._

import Stones._
import Const._
import shared._ // how to import just the package baduk 'import baduk' results in compile error
import shared.Board._


object JSBoard extends js.JSApp {
  val jsonHeaders = Map("Accept" -> "application/json", "Content-Type" -> "application/json")

  def main(): Unit = {
    val canvas = dom.document.getElementById("mainCanvas").asInstanceOf[html.Canvas]
    implicit val boardSize = BoardSize(5, 5)
    val game = Game(boardSize)
    Ajax.post("/games", write(boardSize), headers=jsonHeaders) // inform backend about new game
    val boardCanvas = new BoardCanvas(canvas, boardSize) // draws the empty board

    var player = BLACK
    val invalidCoord = new Coord(-1, -1)
    var lastCoord = invalidCoord
    var lastType = UNKNOWN

    //def debug(): Unit = {
    //  // remove stones div from board.scala.html; keep this debug method until xmas
    //  val stonesDiv = dom.document.getElementById("stones")
    //  stonesDiv.innerHTML = ""
    //  val p1 = dom.document.createElement("p")
    //  p1.innerHTML = "last coord: %s; index: %d".format(lastCoord, lastCoord.toInt)
    //  val p2 = dom.document.createElement("p")
    //  p2.innerHTML = "x: %d; y: %d".format(lastCoord.x, lastCoord.y)
    //  val p3 = dom.document.createElement("p")
    //  p3.innerHTML = "stones: %s".format(game.board.position.toList.toString)

    //  for (p <- List(p1, p2)) { stonesDiv.appendChild(p) }
    //}

    val alertTag = dom.document.createElement("div")
    alertTag.innerHTML = "Alerts come here"

    // add click listener
    def handleClickEvent(ev: dom.MouseEvent): Unit = {
      val coord = boardCanvas.getCoord(ev.clientX, ev.clientY)

      game.check(Move(coord, player)) match {
        case Success(move) => {
          game.make(move)
          Ajax.post("/moves", write(move), headers=jsonHeaders)
          lastType = UNKNOWN
          boardCanvas.draw(game.board)
          player = Game.opponent(player)
          alertTag.innerHTML = "ok"
        }
        case Failure(ex) => {
          alertTag.innerHTML = "illegal Move: %s".format(ex.getMessage)
        }
      }
      //debug()
    }
    canvas.addEventListener("click", handleClickEvent _)

    def handleMousemoveEvent(ev: dom.MouseEvent): Unit = {
      val c = boardCanvas.getCoord(ev.clientX, ev.clientY)
      val currentType = game.board.getStone(c)

      if (lastCoord == c) { return }
      if (lastCoord.isValid && lastType != UNKNOWN) { game.board.setStone(lastCoord, lastType) }
      lastType = game.board.getStone(c)
      lastCoord = c
      // make this more general, i.e. when in "erase mode"
      if (game.board.getStone(c) == EMPTY) {

        val semi = player match { case `BLACK` => SEMI_BLACK; case `WHITE` => SEMI_WHITE; case _ => player }
        game.board.setStone(c, semi)
      }
      boardCanvas.draw(game.board)
      //debug()
    }
    canvas.addEventListener("mousemove", handleMousemoveEvent _)

    def handleMouseoutEvent(ev: dom.MouseEvent): Unit = {
      if (lastCoord.isValid && lastType != UNKNOWN) { game.board.setStone(lastCoord, lastType) }
      lastType = UNKNOWN
      lastCoord = invalidCoord
      boardCanvas.draw(game.board)
      //debug()
    }
    canvas.addEventListener("mouseout", handleMouseoutEvent _)

    // keep this for quick feedback:
    dom.document.getElementById("footer").appendChild(alertTag)
  }
}

