

package twodee.geometry

/**
 * @author mhartl
 */
class Line(p1c : Vector, p2c : Vector) {
  val p1 = p1c
  val p2 = p2c
  
  def slope : Double = (p1.y - p2.y) / (p1.x - p2.x)
   
  def detWith(v : Vector) = {
    Matrix2(v.x - p1.x, v.y - p1.y,
            p2.x - p1.x, p2.y - p1.y).det
  }
  
  def on (v : Vector): Boolean = detWith(v) == 0

  def sameSide (v1 : Vector, v2 : Vector) = Math.signum(detWith(v1)) == Math.signum(detWith(v2))
  
  
  override def toString : String = {
    "l["+p1+" -> "+p2+"]"
  }
}

object Line {
  def apply(p1: Vector, p2: Vector): Line = {
    new Line(p1, p2)
  }
}
