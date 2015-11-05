package board

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html


// object ScalaJSExample extends js.JSApp {
@js.native
class HTMLImageElement extends dom.raw.HTMLImageElement {
  // override var onload: js.Function1[dom.Event, _] = ???
}


@JSExport
object ScalaJSBoard {
  //def main(): Unit = {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // start with a 9x9 board
    val sizeX = 9
    val sizeY = 9
    val padLeft = 5
    val padRight = 5
    val padTop = 5
    val padBottom = 5
    val marginLeft = 25
    val marginRight = 25
    val marginTop = 25
    val marginBottom = 25

    val gridX = 50;
    val gridY = 50;
    val gridTop = padTop + marginTop;
    val gridBottom = gridTop + sizeY * gridY;
    val gridLeft = padLeft + marginLeft;
    val gridRight = gridLeft + sizeX * gridX;

    val boardWidth = marginLeft + sizeX * gridX + marginRight
    val boardHeight = marginTop + sizeY * gridY + marginBottom

    canvas.width = padLeft + boardWidth + padRight
    canvas.height = padTop + boardHeight + padBottom
    //ctx.fillRect(5, 25, 50, 100)
    //grid: {color: '#202020', x: 50, y: 50, smooth: 0.0,
    //    borderWidth: 1.5, lineWidth: 1.2},
    val gridColor = "#101010"

    ctx.strokeStyle = gridColor
    ctx.lineWidth = 1

    // draw material board
    // val imageBoard = dom.document.createElement("img")
    var bgReady = false;
    val bgImage = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    //bgImage.onload = (e: dom.Event) => {
    //  bgReady = true
    //}
    bgImage.src = "images/large/walnut.jpg"
    //val imageBoard = new dom.HTMLImageElement("img")
    //imageBoard.src = "images/large/walnut.png"

    ctx.drawImage(bgImage, 0, 0,
        boardWidth, boardHeight,
        marginLeft, marginTop,
        boardWidth, boardHeight);
    //ctx.stroke()

    // horizontal lines
    ctx.beginPath()
    for(i <- 0 to sizeY) {
      ctx.moveTo(gridLeft, gridTop + i * gridY)
      ctx.lineTo(gridRight, gridTop + i * gridY)
    }
    ctx.stroke()

    // vertical lines
    ctx.beginPath()
    for(i <- 0 to sizeX) {
      ctx.moveTo(gridLeft + i * gridX, gridTop)
      ctx.lineTo(gridLeft + i * gridX, gridBottom)
    }
    ctx.stroke()

    // keep this for quick feedback:
    val bounds = canvas.getBoundingClientRect()
    val paragraph = dom.document.createElement("p")
    paragraph.innerHTML = "Height: %d Width: %d".format(bounds.height, bounds.width)
    val q = dom.document.createElement("p")
    q.innerHTML = "Top: %f, Right: %f, Bottom: %f, Left %f".format(bounds.top, bounds.right, bounds.bottom, bounds.left)
    dom.document.getElementById("footer").appendChild(paragraph)
    dom.document.getElementById("footer").appendChild(q)
    dom.document.getElementById("footer").appendChild(bgImage)
  }

  /** Computes the square of an integer.
   *  This demonstrates unit testing.
   */
  def square(x: Int): Int = x*x
}

