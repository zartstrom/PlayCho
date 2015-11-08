package board

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import Board._

// object ScalaJSExample extends js.JSApp {
@js.native
class HTMLImageElement extends dom.raw.HTMLImageElement {
  //def onload: js.Function1[dom.Event, _] = ???
}


@JSExport
object JSBoard {
  //def main(): Unit = {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // start with a 9x9 board
    implicit val boardSize = BoardSize(5, 5)
    val board = new Board(boardSize)
    val pad = 30
    val padLeft = pad
    val padRight = pad
    val padTop = pad
    val padBottom = pad
    val margin = 40
    val marginLeft = margin
    val marginRight = margin
    val marginTop = margin
    val marginBottom = margin

    val gridX = 50;
    val gridY = 50;
    val gridTop = padTop + marginTop;
    val gridBottom = gridTop + (boardSize.y - 1) * gridY;
    val gridLeft = padLeft + marginLeft;
    val gridRight = gridLeft + (boardSize.x - 1) * gridX;

    val stoneRadius = 24

    val boardWidth = marginLeft + (boardSize.x - 1) * gridX + marginRight
    val boardHeight = marginTop + (boardSize.y - 1) * gridY + marginBottom

    var player = BLACK

    def getX(i: Int): Int = { gridLeft + i * gridX }  // convert a coordinate x-value into pixel x-value on canvas
    def getY(j: Int): Int = { gridTop + j * gridY }

    canvas.width = padLeft + boardWidth + padRight
    canvas.height = padTop + boardHeight + padBottom

    def drawGrid() {
      ctx.globalAlpha = 1
      val gridColor = "#000000"
      ctx.strokeStyle = gridColor
      ctx.lineWidth = 2

      // horizontal lines
      ctx.beginPath()
      for(i <- 0 until boardSize.y) {
        ctx.moveTo(gridLeft, gridTop + i * gridY)
        ctx.lineTo(gridRight, gridTop + i * gridY)
      }
      ctx.stroke()

      // vertical lines
      ctx.beginPath()
      for(i <- 0 until boardSize.x) {
        ctx.moveTo(gridLeft + i * gridX, gridTop)
        ctx.lineTo(gridLeft + i * gridX, gridBottom)
      }
      ctx.stroke()
    }

    def drawStone(c: Coordinate, stone: Int) {
      def paint(alpha: Double, fillStyle: String) {
        ctx.globalAlpha = alpha
        ctx.fillStyle = fillStyle
        ctx.beginPath()
        ctx.arc(getX(c.x), getY(c.y), stoneRadius, 0, 2 * math.Pi);
        ctx.fill()
      }
      stone match {
        case `BLACK` => paint(1, "#000000")
        case `WHITE` => paint(1, "#FFFFFF")
        case `SEMI_BLACK` => paint(0.6, "#000000")
        case `SEMI_WHITE` => paint(0.6, "#FFFFFF")
        case `EMPTY` => { }
        case _ => {}
      }
    }

    def drawStones(board: Board) {
      for (i <- 0 until board.stones.size) {
        val c = board.getCoordinateByPoint(i)
        drawStone(c, board.stones(i))
      }
    }

    def drawBoard() {
      // draw material board
      // var bgReady = false;
      val bgImage = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      bgImage.src = "images/large/walnut.jpg"
      bgImage.onload = (e: dom.Event) => {
        // bgReady = true
        ctx.drawImage(bgImage, padLeft, padTop, boardWidth, boardHeight)
        drawGrid()
        drawStones(board)
      }
    }
    drawBoard()

    //
    def getCoordinate(pageX: Double, pageY: Double): Coordinate = {
      var bounds = canvas.getBoundingClientRect()
      // why canvas.width / (bounds.right - bounds.left)? isn't it == 1?!
      val scaledX = (pageX - bounds.left) * canvas.width / (bounds.right - bounds.left)
      val scaledY = (pageY - bounds.top) * canvas.height / (bounds.bottom - bounds.top)

      val c = new Coordinate(
        math.round((scaledX - marginLeft - padLeft) / gridX).toInt, // math.round returns Long, we want Int
        math.round((scaledY - marginTop - padTop) / gridY).toInt
      )
      c
    }

    //var ev = { type: 'type', coordinate: c, board: this,
    //  oldVal: old, newVal: t };

    var lastCoordinate = new Coordinate(-1, -1)
    var lastType = UNKNOWN

    // add click listener
    val coordsTag = dom.document.createElement("div")
    coordsTag.innerHTML = "Click on the board to find about coords"

    def handleClickEvent(ev: dom.MouseEvent): Unit = {
      val c = getCoordinate(ev.clientX, ev.clientY)
      coordsTag.innerHTML = "ClientX: %d ClientY: %d <br> Coordinate: %s".format(ev.clientX, ev.clientY, c.toString)
      //drawStone(c, Board.WHITE)
      board.setStone(c, player)
      lastType = UNKNOWN
      drawBoard()
      // alternate Player
      player = player match { case `BLACK` => WHITE; case `WHITE` => BLACK; case _ => player }
    }
    canvas.addEventListener("click", handleClickEvent _)

    def handleOnmousemoveEvent(ev: dom.MouseEvent): Unit = {
      val c = getCoordinate(ev.clientX, ev.clientY)
      val currentType = board.getStone(c)

      if (lastCoordinate == c) { return }
      if (lastCoordinate.isValid && lastType != UNKNOWN) { board.setStone(lastCoordinate, lastType) }
      if (board.getStone(c) != EMPTY) { return }

      lastType = board.getStone(c)
      lastCoordinate = c

      val semi = player match { case `BLACK` => SEMI_BLACK; case `WHITE` => SEMI_WHITE; case _ => player }
      board.setStone(c, semi)
      drawBoard()
    }
    canvas.addEventListener("mousemove", handleOnmousemoveEvent _)
    //      ctx.fillStyle = (type == C.WHITE) ? '#FFFFFF' : '#000000';
    //ctx.beginPath();
    //ctx.arc(ox, oy, this.stoneR*scale, 2*Math.PI, false);
    //ctx.fill();

    //if(type == C.WHITE) {
    //  ctx.strokeStyle = '#000000';
    //  ctx.stroke();
    //}


  // Stones and marks
  //jboard.each(function(c, type, mark) {
  //  var ox = (this.getX(c.i - this.opt.view.xOffset));
  //  var oy = (this.getY(c.j - this.opt.view.yOffset));
  //  var markColor;

  //  switch(type) {
  //    case C.BLACK:
  //    case C.DIM_BLACK:
  //      this.ctx.globalAlpha = type == C.BLACK ? 1 : this.opt.stone.dimAlpha;
  //      this.stones.drawStone(this.ctx, type, ox, oy);
  //      markColor = this.opt.mark.blackColor; // if we have marks, this is the color
  //      break;
  //    case C.WHITE:
  //    case C.DIM_WHITE:
  //      this.ctx.globalAlpha = type == C.WHITE ? 1 : this.opt.stone.dimAlpha;
  //      this.stones.drawStone(this.ctx, type, ox, oy);
  //      markColor = this.opt.mark.whiteColor; // if we have marks, this is the color
  //      break;
  //    default:
  //      this.ctx.globalAlpha=1;
  //      markColor = this.opt.mark.clearColor; // if we have marks, this is the color
  //  }

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

  /** Computes the square of an integer.
   *  This demonstrates unit testing.
   */
  def square(x: Int): Int = x*x
}

