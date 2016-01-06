package twodee.util

import twodee.shapes._
import twodee.transform._

/**
 * @author mhartl
 */
object Test {
  def main(args: Array[String]) {
    val s = Circle.unit - Circle.unit.scale(0.2) - RegularPolygon.square.scale(0.1).translate(-0.8,0)
//    val s1 = s.rotate(Rad(Math.PI/2)) // .translate(0.5, 0).rotateAroundOrigin(Rad(Math.PI/2))
    print(DrawShape((s), new Raster(40, Box.stdBox))) // Box.boundingBox(s.boundingPoly))))
  }
}
