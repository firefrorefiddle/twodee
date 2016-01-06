package shapes

import geometry._
import transform._

/**
 * @author rmh
 */
class Polygon (ccentre : Vector, cpoints : List[Vector]) extends Shape {

  val centre = ccentre
  val points = cpoints

  def edges: List[Line] =
    points.zip(points.tail ++ points).map(pair => Line(pair._1, pair._2))

  def inside(p: Vector) = edges.forall(e => e.sameSide(p, centre))
  
  def boundingPoly: List[Vector] = points
  
  def transform(trans : Transformation) = Polygon(trans(centre), points.map(p => trans(p)))
}

object Polygon {
  def apply(centre : Vector, points : List[Vector]) = new Polygon(centre, points)
}