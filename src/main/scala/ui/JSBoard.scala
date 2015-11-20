package ui

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.{Try, Success, Failure}

import Stones._
import Const._
import baduk._ // how to import just the package baduk 'import baduk' results in compile error
import baduk.Board._


@JSExport
object JSBoard {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    implicit val boardSize = BoardSize(9, 9)
    val game = Game(boardSize)
    val boardCanvas = new BoardCanvas(canvas, boardSize) // draws the empty board

    var player = BLACK
    val invalidCoord = new Coord(-1, -1)
    var lastCoord = invalidCoord
    var lastType = UNKNOWN

    def debug(): Unit = {
      val stonesDiv = dom.document.getElementById("stones")
      stonesDiv.innerHTML = ""
      val p1 = dom.document.createElement("p")
      p1.innerHTML = "last coord: %s; index: %d".format(lastCoord, lastCoord.toInt)
      val p2 = dom.document.createElement("p")
      p2.innerHTML = "x: %d; y: %d".format(lastCoord.x, lastCoord.y)
      val p3 = dom.document.createElement("p")
      p3.innerHTML = "stones: %s".format(game.board.position.toList.toString)

      for (p <- List(p1, p2)) { stonesDiv.appendChild(p) }
    }

    // add click listener
    val coordsTag = dom.document.createElement("div")
    coordsTag.innerHTML = "Click on the board to find about coords"
    val alertTag = dom.document.createElement("div")
    alertTag.innerHTML = "Alerts come here"

    def handleClickEvent(ev: dom.MouseEvent): Unit = {
      val coord = boardCanvas.getCoord(ev.clientX, ev.clientY)
      coordsTag.innerHTML = "ClientX: %d ClientY: %d <br> Coord: %s".format(ev.clientX, ev.clientY, coord.toString)

      game.check(Move(coord, player))(game.board.position) match {
        case Success(move) => {
          game.make(move)(game.board.position) // ugly, make game.board.position implicit?! 
          lastType = UNKNOWN
          boardCanvas.draw(game.board)
          player = Game.opponent(player)
          alertTag.innerHTML = "ok"
        }
        case Failure(ex) => {
          alertTag.innerHTML = "illegal Move: %s".format(ex.getMessage)
        }
      }
      debug()
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
      debug()
    }
    canvas.addEventListener("mousemove", handleMousemoveEvent _)

    def handleMouseoutEvent(ev: dom.MouseEvent): Unit = {
      if (lastCoord.isValid && lastType != UNKNOWN) { game.board.setStone(lastCoord, lastType) }
      lastType = UNKNOWN
      lastCoord = invalidCoord
      boardCanvas.draw(game.board)
      debug()
    }
    canvas.addEventListener("mouseout", handleMouseoutEvent _)

    // keep this for quick feedback:
    val bounds = canvas.getBoundingClientRect()
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = "Height: %d Width: %d".format(bounds.height, bounds.width)
    val q = dom.document.createElement("p")
    q.innerHTML = "Top: %f, Right: %f, Bottom: %f, Left %f".format(bounds.top, bounds.right, bounds.bottom, bounds.left)
    dom.document.getElementById("footer").appendChild(alertTag)
    dom.document.getElementById("footer").appendChild(paragraph)
    dom.document.getElementById("footer").appendChild(q)
    dom.document.getElementById("footer").appendChild(coordsTag)
    //dom.document.getElementById("footer").appendChild(bgImage)
  }
}

