package board

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

import Board._
import Stones._

@js.native
class HTMLImageElement extends dom.raw.HTMLImageElement {
  //def onload: js.Function1[dom.Event, _] = ???
}

@js.native
class HTMLCanvasElement extends dom.raw.HTMLCanvasElement { }


@JSExport
object JSBoard {
  //def main(): Unit = {
  @JSExport
  def main(canvas: html.Canvas): Unit = {
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

    // start with a 9x9 board
    implicit val boardSize = BoardSize(13, 13)
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
    //val shadowHeight, shadowWidth = 2 * stoneRadius
    ctx.shadowColor = "#ffe0a8"
    ctx.shadowBlur = 30
    ctx.shadowOffsetX = 5
    ctx.shadowOffsetX = 5
    val shadowOffX = 5
    val shadowOffY = 5

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
    //  // Store rendered board in another canvas for fast redraw
    //  this.backup = document.createElement('canvas');
    //  this.backup.width = canvas.width;
    //  this.backup.height = canvas.height;
    //  this.backup.getContext('2d').drawImage(canvas,
    //      0, 0, canvas.width, canvas.height,
    //      0, 0, canvas.width, canvas.height);

    var backup = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]

    def createBackup(): Unit = {
      backup.width = canvas.width
      backup.height = canvas.height
      backup.getContext("2d").drawImage(
        canvas, padLeft, padTop, boardWidth, boardHeight, padLeft, padTop, boardWidth, boardHeight
      )
    }

    def drawBoard() {
      // draw material board
      val bgImage = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
      bgImage.src = "images/large/walnut.jpg"
      bgImage.onload = (e: dom.Event) => {
        ctx.drawImage(bgImage, padLeft, padTop, boardWidth, boardHeight)
        drawGrid()
        createBackup()
      }
    }
    drawBoard()

    def drawShadow(c: Coordinate): Unit = {
      val x = getX(c.x) + shadowOffX
      val y = getY(c.y) + shadowOffY
      ctx.globalAlpha = 1
      ctx.drawImage(shadowOfStone,
        (x - shadowOfStone.width / 2),
        (y - shadowOfStone.height / 2),
        shadowOfStone.width, shadowOfStone.height
      );
    }

    def drawMaterialStone(stone: HTMLImageElement, c: Coordinate, alpha: Double) {
      val x = getX(c.x)
      val y = getY(c.y)
      ctx.globalAlpha = alpha
      ctx.drawImage(stone, 0, 0, 2 * stoneRadius, 2 * stoneRadius,
        (x - stoneRadius),
        (y - stoneRadius),
        2 * stoneRadius, 2 * stoneRadius
      );
    }

    def drawStone(coord: Coordinate, player: Int): Unit = {
      player match {
        case `BLACK` => { drawShadow(coord); drawMaterialStone(blackStone, coord, 1) }
        case `WHITE` => { drawShadow(coord); drawMaterialStone(whiteStone, coord, 1) }
        case `SEMI_BLACK` => drawMaterialStone(blackStone, coord, 0.6)
        case `SEMI_WHITE` => drawMaterialStone(whiteStone, coord, 0.6)
        case `EMPTY` => { }
        case _ => {}
      }
    }

    def drawStones(board: Board): Unit = {
      for (i <- 0 until board.stones.size) {
        val c = board.getCoordinateByPoint(i)
        drawStone(c, board.stones(i))
      }
    }

    def draw(): Unit = {
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      ctx.globalAlpha = 1
      ctx.drawImage(
        backup, padLeft, padTop, boardWidth, boardHeight, padLeft, padTop, boardWidth, boardHeight
      )
      drawStones(board)
    }

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
      val c = getCoordinate(ev.clientX, ev.clientY)
      coordsTag.innerHTML = "ClientX: %d ClientY: %d <br> Coordinate: %s".format(ev.clientX, ev.clientY, c.toString)
      //drawStone(c, Board.WHITE)
      board.setStone(c, player)
      lastType = UNKNOWN
      draw()
      // alternate Player
      player = player match { case `BLACK` => WHITE; case `WHITE` => BLACK; case _ => player }
      debug()
    }
    canvas.addEventListener("click", handleClickEvent _)

    def handleMousemoveEvent(ev: dom.MouseEvent): Unit = {
      val c = getCoordinate(ev.clientX, ev.clientY)
      val currentType = board.getStone(c)

      if (lastCoordinate == c) { return }
      if (lastCoordinate.isValid && lastType != UNKNOWN) { board.setStone(lastCoordinate, lastType) }
      // make this more general, i.e. when in "erase mode"
      if (board.getStone(c) == EMPTY) {
        lastType = board.getStone(c)
        lastCoordinate = c

        val semi = player match { case `BLACK` => SEMI_BLACK; case `WHITE` => SEMI_WHITE; case _ => player }
        board.setStone(c, semi)
      }
      draw()
      debug()
    }
    canvas.addEventListener("mousemove", handleMousemoveEvent _)

    def handleMouseoutEvent(ev: dom.MouseEvent): Unit = {
      //val c = getCoordinate(ev.clientX, ev.clientY)  // duplicate code in the event handlers
      if (lastCoordinate.isValid && lastType != UNKNOWN) { board.setStone(lastCoordinate, lastType) }
      lastType = UNKNOWN
      lastCoordinate = invalidCoord
      draw()
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

  /** Computes the square of an integer.
   *  This demonstrates unit testing.
   */
  def square(x: Int): Int = x*x
}

