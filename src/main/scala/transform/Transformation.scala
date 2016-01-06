package transform

import geometry.Vector
import geometry.Matrix3
import shapes._

abstract class Angle {
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

trait Transformable[T] {

  def transform(trans: Transformation): T
  def centre: Vector

  def rotate(theta: Angle) = transform(Rotate(theta, centre))
  def rotateAroundOrigin(theta: Angle) = transform(Rotate(theta))
  def translate(tv: Vector) = transform(Translate(tv))
  def translate(tx: Double, ty: Double) = transform(Translate(tx, ty))
  def scale(c: Double) = transform(Scale(c, centre))
  def scale(cx: Double, cy: Double) = transform(Scale(cx, cy, centre))
  def scaleFromOrigin(c: Double) = transform(Scale(c))
  def scaleFromOrigin(cx: Double, cy: Double) = transform(Scale(cx, cy))
  def shear(sx: Double, sy: Double) = transform(Shear(sx, sy))
}

/**
 * @author rmh
 */
class Transformation(ctransM: Matrix3, cinvM: Matrix3) {
  val transM = ctransM
  val invM = cinvM

  def transform(v: Vector): Vector = transM ** v
  def invTransform(v: Vector): Vector = invM ** v

  def <+(next: Transformation) =
    new Transformation(transM * next.transM, next.invM * invM)

  def +>(next: Transformation) =
    new Transformation(next.transM * transM, invM * next.invM)

  def apply(v: Vector) = transform(v)
  def apply(s: Shape) = s.transform(this)

  override def toString = "T " + transM + " I " + invM
}

object Transformation {
  private def scaleM(cx: Double, cy: Double): Matrix3 =
    new Matrix3(
      cx, 0, 0,
      0, cy, 0,
      0, 0, 1)

  private def translateM(tx: Double, ty: Double): Matrix3 =
    new Matrix3(
      1, 0, tx,
      0, 1, ty,
      0, 0, 1)

  private def shearM(sx: Double, sy: Double): Matrix3 =
    new Matrix3(
      1, sx, 0,
      sy, 1, 0,
      0, 0, 1)

  private def translateM(tv: Vector): Matrix3 = translateM(tv.x, tv.y)

  private def rotateM(rad: Double): Matrix3 =
    new Matrix3(
      Math.cos(rad), -Math.sin(rad), 0,
      Math.sin(rad), Math.cos(rad), 0,
      0, 0, 1)

  def scaleFromOrigin(cx: Double, cy: Double) = new Transformation(scaleM(cx, cy), scaleM(1 / cx, 1 / cy))
  def scaleFromOrigin(c: Double): Transformation = scaleFromOrigin(c, c)
  def scaleXFromOrigin(cx: Double) = scaleFromOrigin(cx, 1)
  def scaleYFromOrigin(cy: Double) = scaleFromOrigin(1, cy)

  def translate(tx: Double, ty: Double) = new Transformation(translateM(tx, ty), translateM(-tx, -ty))
  def translate(tv: Vector) = new Transformation(translateM(tv), translateM(-tv))

  def rotateAroundOrigin(theta: Angle) = new Transformation(rotateM(theta.rad), rotateM(-theta.rad))

  def shear(sx: Double, sy: Double) = new Transformation(shearM(sx, sy), shearM(-sx, -sy))

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