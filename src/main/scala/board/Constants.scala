package board

import org.scalajs.dom


object Stones {

  val whiteStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  whiteStone.src = "images/large/white.png"

  var blackStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  blackStone.src = "images/large/black.png"

  var shadowOfStone = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
  shadowOfStone.src = "images/large/shadow_dark.png"

}
