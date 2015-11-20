package ui

import org.scalajs.dom


object Stones {

  val whiteStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  whiteStone.src = "/assets/images/large/white.png"

  val blackStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  blackStone.src = "/assets/images/large/black.png"

  val shadowOfStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  shadowOfStone.src = "/assets/images/large/shadow_dark.png"

}

object Const {

  val COORDS = Array[String]("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O")  // and so on

  val pad = 40
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

  val stoneRadius = 24
  val shadowOffX = 7
  val shadowOffY = 7

  val gridColor = "#000000"
}
