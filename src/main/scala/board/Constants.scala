package board

import org.scalajs.dom


object Stones {

  val whiteStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  whiteStone.src = "images/large/white.png"

  val blackStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  blackStone.src = "images/large/black.png"

  val shadowOfStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  shadowOfStone.src = "images/large/shadow_dark.png"

}

object Constants {

  val COORDINATES = Array[String]("A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O")  // and so on

}
