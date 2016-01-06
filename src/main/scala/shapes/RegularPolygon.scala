

package shapes

import geometry._
import transform._

/**
 * @author mhartl
 */
object RegularPolygon {
  def apply(nPoints: Int) =
    Polygon(Vector(0, 0),
      for (i <- List.range(0, 360, 360 / nPoints))
        yield Vector(1, 0).transform(Rotate(Deg(i))))
        
  def square = RegularPolygon(4).rotate(Deg(45))
}