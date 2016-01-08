package twodee.transform

import twodee.geometry.Vector
import twodee.geometry.Matrix3
import twodee.shapes._

trait Angle {
  def rad: Double
  def deg: Double
}

case class Deg(a: Double) extends Angle {
  def rad = a / 180 * Math.PI
  def deg = a
}

case class Rad(a: Double) extends Angle {
  def deg = a / Math.PI * 180
  def rad = a
}

/**
 * Anything that can be transformed must specify
 * its center for the rotate and scale operations
 * to work correctly.
 */
trait Transformable[T] {

  /**
   * Transform the object by a certain transformation.
   */
  def transform(trans: Transformation): T

  /**
   * The center point for rotating and scaling.
   */
  def centre: Vector

  /**
   * Rotate around center, which is done by first translating
   * to the origin, then rotating around the origin, then translating
   * back.
   */
  def rotate(theta: Angle)                    = transform(Rotate(theta, centre))
  def rotateAroundOrigin(theta: Angle)        = transform(Rotate(theta))
  def translate(tv: Vector)                   = transform(Translate(tv))
  def translate(tx: Double, ty: Double)       = transform(Translate(tx, ty))

  /**
   * scale from center, which is done by first translating to the origin,
   * then scaling from the origin, then translating back.
   */
  def scale(c: Double)                        = transform(Scale(c, centre))
  def scale(cx: Double, cy: Double)           = transform(Scale(cx, cy, centre))
  def scaleFromOrigin(c: Double)              = transform(Scale(c))
  def scaleFromOrigin(cx: Double, cy: Double) = transform(Scale(cx, cy))
  def shear(sx: Double, sy: Double)           = transform(Shear(sx, sy))
}

/**
 * A transformation is defined by a transformation matrix
 * as well as the inverse matrix to unapply it, which is
 * sometimes more convenient, when we ask: "if this point were
 * transformed, would it be inside this transformed shape?"
 * rather than "where is this transformed point?"
 *
 * For efficiency reasons, it is left to the userto supply the
 * correct inverse matrix rather than calculating
 * it from the original.
 */
case class Transformation(transM: Matrix3, invM: Matrix3) {

  def transform   (v: Vector): Vector = transM ** v
  def invTransform(v: Vector): Vector = invM   ** v

  /**
   * Create a new transformation which is defined by first
   * applying this one and then the next.
   */
  def <+(next: Transformation) =
    new Transformation(transM * next.transM, next.invM * invM)

  /**
   * Create a new transformation which is defined by first
   * applying this one after another.
   */
  def +>(previous: Transformation) =
    new Transformation(previous.transM * transM, invM * previous.invM)

  def apply(v: Vector)              = transform(v)
  def apply[T](t: Transformable[T]) = t.transform(this)

  override def toString = "T " + transM + " I " + invM
}

object Transformation {

  /**
   * Create a matrix for scaling a vector in two dimensions.
   */
  private def scaleM(cx: Double, cy: Double): Matrix3 =
    new Matrix3(
      cx, 0, 0,
      0, cy, 0,
      0, 0, 1)

  /**
   * Create a matrix for scaling a vector by an x and y offset.
   */
  private def translateM(tx: Double, ty: Double): Matrix3 =
    new Matrix3(
      1, 0, tx,
      0, 1, ty,
      0, 0, 1)

  /**
   * Create a matrix for scaling a vector by another vector.
   * 
   *     translateM(x,y) == translateM(Vector(x,y))
   */
  private def translateM(tv: Vector): Matrix3 = translateM(tv.x, tv.y)
 
  /**
   * Create a matrix for shearing a vector in two directions.
   */
  private def shearM(sx: Double, sy: Double): Matrix3 =
    new Matrix3(
      1, sx, 0,
      sy, 1, 0,
      0, 0, 1)

  /**
   * Create a matrix for rotating a vector by an angle specified in radians.
   */
  private def rotateM(rad: Double): Matrix3 =
    new Matrix3(
      Math.cos(rad), -Math.sin(rad), 0,
      Math.sin(rad), Math.cos(rad), 0,
      0, 0, 1)

  def scaleFromOrigin(cx: Double, cy: Double): Transformation =
      new Transformation(scaleM(cx, cy), scaleM(1 / cx, 1 / cy))
  def scaleFromOrigin(c: Double): Transformation =
      scaleFromOrigin(c, c)
  def scaleXFromOrigin(cx: Double)            = scaleFromOrigin(cx, 1)
  def scaleYFromOrigin(cy: Double)            = scaleFromOrigin(1, cy)

  def translate(tx: Double, ty: Double) = new Transformation(translateM(tx, ty), translateM(-tx, -ty))
  def translate(tv: Vector)             = new Transformation(translateM(tv),     translateM(-tv))

  def rotateAroundOrigin(theta: Angle)  = new Transformation(rotateM(theta.rad), rotateM(-theta.rad))

  def shear(sx: Double, sy: Double)     = new Transformation(shearM(sx, sy), shearM(-sx, -sy))

  def id = new Transformation(Matrix3.unit, Matrix3.unit)
}

object Rotate {
  def apply(theta: Angle) = Transformation.rotateAroundOrigin(theta)
  def apply(theta: Angle, origin: Vector) =
    Translate(-origin) +> Transformation.rotateAroundOrigin(theta) +> Translate(origin)
}

object Translate {
  def apply(tx: Double, ty: Double) = Transformation.translate(tx, ty)
  def apply(tv: Vector) = Transformation.translate(tv)
}

object Scale {
  def apply(c: Double) = Transformation.scaleFromOrigin(c)
  def apply(cx: Double, cy: Double) = Transformation.scaleFromOrigin(cx, cy)
  def apply(c: Double, origin: Vector) =
    Translate(-origin) +> Transformation.scaleFromOrigin(c) +> Translate(origin)
  def apply(cx: Double, cy: Double, origin: Vector) =
    Translate(-origin) +> Transformation.scaleFromOrigin(cx, cy) +> Translate(origin)
}

object Shear {
  def apply(cx: Double, cy: Double) = Transformation.shear(cx, cy)
}
