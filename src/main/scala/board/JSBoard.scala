package board

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import Board._
import Stones._
import Const._


@JSExport
object JSBoard {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    implicit val boardSize = BoardSize(13, 5)
    val board = new Board(boardSize)
    val boardCanvas = new BoardCanvas(canvas, boardSize) // draws the empty board

    var player = BLACK
    val invalidCoord = new Coordinate(-1, -1)
    var lastCoordinate = invalidCoord
    var lastType = UNKNOWN

    def debug(): Unit = {
      val stonesDiv = dom.document.getElementById("stones")
      stonesDiv.innerHTML = ""
      val p1 = dom.document.createElement("p")
      p1.innerHTML = "last coordinate: %s; index: %d".format(lastCoordinate, lastCoordinate.toIndex)
      val p2 = dom.document.createElement("p")
      p2.innerHTML = "x: %d; y: %d".format(lastCoordinate.x, lastCoordinate.y)
      val p3 = dom.document.createElement("p")
      p3.innerHTML = "stones: %s".format(board.stones.toList.toString)

      for (p <- List(p1, p2)) { stonesDiv.appendChild(p) }
    }

    // add click listener
    val coordsTag = dom.document.createElement("div")
    coordsTag.innerHTML = "Click on the board to find about coords"

    def handleClickEvent(ev: dom.MouseEvent): Unit = {
      val c = boardCanvas.getCoordinate(ev.clientX, ev.clientY)
      coordsTag.innerHTML = "ClientX: %d ClientY: %d <br> Coordinate: %s".format(ev.clientX, ev.clientY, c.toString)
      board.setStone(c, player)
      lastType = UNKNOWN
      boardCanvas.draw(board)
      player = player match { case `BLACK` => WHITE; case `WHITE` => BLACK; case _ => player }
      debug()
    }
    canvas.addEventListener("click", handleClickEvent _)

    def handleMousemoveEvent(ev: dom.MouseEvent): Unit = {
      val c = boardCanvas.getCoordinate(ev.clientX, ev.clientY)
      val currentType = board.getStone(c)

      if (lastCoordinate == c) { return }
      if (lastCoordinate.isValid && lastType != UNKNOWN) { board.setStone(lastCoordinate, lastType) }
      lastType = board.getStone(c)
      lastCoordinate = c
      // make this more general, i.e. when in "erase mode"
      if (board.getStone(c) == EMPTY) {

        val semi = player match { case `BLACK` => SEMI_BLACK; case `WHITE` => SEMI_WHITE; case _ => player }
        board.setStone(c, semi)
      }
      boardCanvas.draw(board)
      debug()
    }
    canvas.addEventListener("mousemove", handleMousemoveEvent _)

    def handleMouseoutEvent(ev: dom.MouseEvent): Unit = {
      //val c = getCoordinate(ev.clientX, ev.clientY)  // duplicate code in the event handlers
      if (lastCoordinate.isValid && lastType != UNKNOWN) { board.setStone(lastCoordinate, lastType) }
      lastType = UNKNOWN
      lastCoordinate = invalidCoord
      boardCanvas.draw(board)
      debug()
    }
    canvas.addEventListener("mouseout", handleMouseoutEvent _)

    // keep this for quick feedback:
    val bounds = canvas.getBoundingClientRect()
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = "Height: %d Width: %d".format(bounds.height, bounds.width)
    val q = dom.document.createElement("p")
    q.innerHTML = "Top: %f, Right: %f, Bottom: %f, Left %f".format(bounds.top, bounds.right, bounds.bottom, bounds.left)
    dom.document.getElementById("footer").appendChild(paragraph)
    dom.document.getElementById("footer").appendChild(q)
    dom.document.getElementById("footer").appendChild(coordsTag)
    //dom.document.getElementById("footer").appendChild(bgImage)
  }
}

