package twodee.geometry

class Matrix2(cx11: Double, cx12: Double,
              cx21: Double, cx22: Double) {
  val x11 = cx11
  val x12 = cx12
  val x21 = cx21
  val x22 = cx22

  def det: Double = x11 * x22 - x12 * x21
}

object Matrix2 {
  def apply(cx11: Double, cx12: Double,
            cx21: Double, cx22: Double) =
    new Matrix2(cx11: Double, cx12: Double,
      cx21: Double, cx22: Double)

}

/**
 * @author rmh
 */
class Matrix3(cx11: Double, cx12: Double, cx13: Double,
              cx21: Double, cx22: Double, cx23: Double,
              cx31: Double, cx32: Double, cx33: Double) {

  val x11 = cx11
  val x12 = cx12
  val x13 = cx13
  val x21 = cx21
  val x22 = cx22
  val x23 = cx23
  val x31 = cx31
  val x32 = cx32
  val x33 = cx33

  def *(o: Matrix3): Matrix3 = {
    new Matrix3(x11 * o.x11 + x12 * o.x21 + x13 * o.x31, x11 * o.x12 + x12 * o.x22 + x13 * o.x32, x11 * o.x13 + x12 * o.x23 + x13 * o.x33,
      x21 * o.x11 + x22 * o.x21 + x23 * o.x31, x21 * o.x12 + x22 * o.x22 + x23 * o.x32, x21 * o.x13 + x22 * o.x23 + x23 * o.x33,
      x31 * o.x11 + x32 * o.x21 + x33 * o.x31, x31 * o.x12 + x32 * o.x22 + x33 * o.x32, x31 * o.x13 + x32 * o.x23 + x33 * o.x33)
  }

  def **(v: Vector): Vector = {
    new Vector(v.x * x11 + v.y * x12 + x13, v.x * x21 + v.y * x22 + x23)
  }

  override def toString = "(" + x11 + "," + x12 + "," + x13 + "/" + x21 + "," + x22 + "," + x23 + "/" + x31 + "," + x32 + "," + x33 + ")"
}

object Matrix3 {
  val unit = new Matrix3(1, 0, 0,
    0, 1, 0,
    0, 0, 1)
}
