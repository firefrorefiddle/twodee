package geometry

import transform._

/**
 * @author rmh
 */
class Vector(cx: Double, cy: Double) extends Transformable[Vector] {
  val x = cx
  val y = cy
  val centre = this

  def *(c: Double) = new Vector(c * x, c * y)
  def /(c: Double) = new Vector(x / c, y / c)
  def /|(c: Double) = new Vector(c / x, c / y)
  def +(v: Vector) = new Vector(x + v.x, y + v.y)
  def unary_- = new Vector(-x, -y)
  override def toString = "<" + x + "/" + y + ">"

  def transform(t: Transformation) = t.transform(this)
  def invTransform(t: Transformation) = t.invTransform(this)
}

object Vector {
  def apply(x: Double, y: Double) = new Vector(x, y)
  val origin = Vector(0, 0)
}