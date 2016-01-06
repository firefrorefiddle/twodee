package twodee.shapes

import twodee.geometry.Vector
import twodee.transform._

/**
 * A circle is totally defined by a transformation to the unit
 * circle.
 */
case class Circle(trans: Transformation) extends Shape {

  private val bounds: List[Vector] =
    List(Vector(-1, 1).transform(trans), Vector(-1, -1).transform(trans),
      Vector(1, -1).transform(trans), Vector(1, 1).transform(trans))

  val centre: Vector = trans.transform(Vector.origin)

  def inside(p: Vector): Boolean = {
    val tp = p.invTransform(trans)
    Math.sqrt(tp.x * tp.x + tp.y * tp.y) <= 1;
  }
  def boundingPoly: List[Vector] = bounds

  def transform(trans: Transformation): Shape = {
    new Circle(this.trans +> trans)
  }
}

object Circle {
  val unit = Circle(Transformation.id)
}
