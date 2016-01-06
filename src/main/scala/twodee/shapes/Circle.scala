package twodee.shapes

import twodee.geometry.Vector
import twodee.transform._
/**
 * @author mhartl
 */
class Circle(ctrans: Transformation) extends Shape {

  private val trans = ctrans
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
  def apply(trans: Transformation) = new Circle(trans)
}
