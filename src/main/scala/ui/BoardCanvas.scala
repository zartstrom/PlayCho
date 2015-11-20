package ui

import org.scalajs.dom
import org.scalajs.dom.html
import scala.scalajs.js

import Stones._
import baduk.Board
import baduk.Board._
import baduk.BoardSize
import baduk.Coord


@js.native
class CanvasContext extends dom.CanvasRenderingContext2D { }

@js.native
class HTMLImageElement extends dom.raw.HTMLImageElement { }

@js.native
class HTMLCanvasElement extends dom.raw.HTMLCanvasElement { }


class BoardCanvas(canvas: html.Canvas, implicit val boardSize: BoardSize) {
  /* constants and function to draw the baduk board */
  val ctx = canvas.getContext("2d").asInstanceOf[CanvasContext]

  val gridTop = Const.padTop + Const.marginTop;
  val gridBottom = gridTop + (boardSize.y - 1) * Const.gridY;
  val gridLeft = Const.padLeft + Const.marginLeft;
  val gridRight = gridLeft + (boardSize.x - 1) * Const.gridX;

  val boardWidth = Const.marginLeft + (boardSize.x - 1) * Const.gridX + Const.marginRight
  val boardHeight = Const.marginTop + (boardSize.y - 1) * Const.gridY + Const.marginBottom

  def getX(i: Int): Int = { gridLeft + i * Const.gridX }  // convert a coord x-value into pixel x-value on canvas
  def getY(j: Int): Int = { gridTop + j * Const.gridY }

  val canvasWidth: Int = { Const.padLeft + boardWidth + Const.padRight }
  val canvasHeight: Int = { Const.padTop + boardHeight + Const.padBottom }

  def grid(): Unit = {
    /* draw grid */
    ctx.globalAlpha = 1
    ctx.strokeStyle = Const.gridColor
    ctx.lineWidth = 2

    // horizontal lines
    ctx.beginPath()
    for(i <- 0 until boardSize.y) {
      ctx.moveTo(gridLeft, gridTop + i * Const.gridY)
      ctx.lineTo(gridRight, gridTop + i * Const.gridY)
    }
    ctx.stroke()

    // vertical lines
    ctx.beginPath()
    for(i <- 0 until boardSize.x) {
      ctx.moveTo(gridLeft + i * Const.gridX, gridTop)
      ctx.lineTo(gridLeft + i * Const.gridX, gridBottom)
    }
    ctx.stroke()
  }

  def coords(): Unit = {
    //   A B C D E F G H J K L ...
    // 1
    // 2
    // ...

    ctx.font = "normal 18px sanf-serif" // move to Const
    ctx.fillStyle = "#808080" // move to Const
    ctx.textAlign = "center"
    ctx.textBaseline = "middle"

    for(i <- 0 until boardSize.x) {
      ctx.fillText(Const.COORDS(i), gridLeft + Const.gridX * i, Const.marginTop / 2)
      ctx.fillText(Const.COORDS(i), gridLeft + Const.gridX * i, canvasHeight - Const.marginTop / 2)
    }
    for(j <- 0 until boardSize.y) {
      ctx.fillText((boardSize.y - j).toString, Const.marginLeft / 2, gridTop + j * Const.gridY)
      ctx.fillText((boardSize.y - j).toString, canvasWidth - Const.marginRight / 2, gridTop + j * Const.gridY)
    }
  }

  def wood(image: HTMLImageElement): Unit = {
    // draw the wooden board
    ctx.shadowBlur = 10
    ctx.shadowOffsetX = 5
    ctx.shadowOffsetY = 5
    ctx.shadowColor = "gray" // move to Const
    ctx.drawImage(image, Const.padLeft, Const.padTop, boardWidth, boardHeight)
    ctx.shadowColor = "transparent"  // clean up afterwards
  }

  var backup = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]

  def createBackup(canvas: html.Canvas): Unit = {
    backup.width = canvasWidth
    backup.height = canvasHeight
    backup.getContext("2d").drawImage(
      canvas,
      Const.padLeft, Const.padTop, boardWidth, boardHeight,
      Const.padLeft, Const.padTop, boardWidth, boardHeight
    )
  }

  def emptyBoard() {
    canvas.width = canvasWidth
    canvas.height = canvasHeight

    val bgImage = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    bgImage.src = "images/large/walnut.jpg"
    bgImage.onload = (e: dom.Event) => {
      wood(bgImage)
      grid()
      coords()
      createBackup(canvas)
    }
  }

  emptyBoard()  // hidden part of the constructor

  def shadow(c: Coord): Unit = {
    val x = getX(c.x) + Const.shadowOffX
    val y = getY(c.y) + Const.shadowOffY
    val shadowScale = 1.8  // this is about the picture "shadowOfStone"
    ctx.globalAlpha = 1
    ctx.drawImage(shadowOfStone,
      (x - Const.gridX * shadowScale / 2),
      (y - Const.gridY * shadowScale / 2),
      Const.gridX * shadowScale, Const.gridY * shadowScale
    );
  }

  def materialStone(stone: HTMLImageElement, c: Coord, alpha: Double) {
    val x = getX(c.x)
    val y = getY(c.y)
    ctx.globalAlpha = alpha
    ctx.drawImage(stone, 0, 0, 2 * Const.stoneRadius, 2 * Const.stoneRadius,
      (x - Const.stoneRadius),
      (y - Const.stoneRadius),
      2 * Const.stoneRadius, 2 * Const.stoneRadius
    );
  }

  def stone(coord: Coord, player: Int): Unit = {
    player match {
      case `BLACK` => { shadow(coord); materialStone(blackStone, coord, 1) }
      case `WHITE` => { shadow(coord); materialStone(whiteStone, coord, 1) }
      case `SEMI_BLACK` => materialStone(blackStone, coord, 0.6)
      case `SEMI_WHITE` => materialStone(whiteStone, coord, 0.6)
      case `EMPTY` => { }
      case _ => {}
    }
  }

  def stones(board: Board): Unit = {
    for (i <- 0 until board.position.size) {
      val c = board.getCoordByPoint(i)
      stone(c, board.position(i))
    }
  }

  def draw(board: Board): Unit = {
    ctx.clearRect(Const.padLeft, Const.padTop, boardWidth, boardHeight)
    ctx.globalAlpha = 1
    ctx.drawImage(
      backup,
      Const.padLeft, Const.padTop, boardWidth, boardHeight,
      Const.padLeft, Const.padTop, boardWidth, boardHeight
    )
    stones(board)
  }

  def getCoord(pageX: Double, pageY: Double): Coord = {
    /** converts a mouse position into baduk board coord */
    var bounds = canvas.getBoundingClientRect()
    val scaledX = (pageX - bounds.left) * canvasWidth / (bounds.right - bounds.left)
    val scaledY = (pageY - bounds.top) * canvasHeight / (bounds.bottom - bounds.top)

    val c = new Coord(
      math.round((scaledX - Const.marginLeft - Const.padLeft) / Const.gridX).toInt, // math.round returns Long, we want Int
      math.round((scaledY - Const.marginTop - Const.padTop) / Const.gridY).toInt
    )
    c
  }
}
