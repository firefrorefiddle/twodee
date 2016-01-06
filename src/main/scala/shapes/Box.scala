

package shapes

import geometry.Vector

/**
 * @author mhartl
 */
class Box(upperLeftC: Vector, lowerRightC: Vector) {
  val upperLeft = upperLeftC
  val lowerRight = lowerRightC

  def width: Double = {
    lowerRight.x - upperLeft.x
  }

  def height: Double = {
    upperLeft.y - lowerRight.y
  }

  def maxDim: Double = {
    List(width, height).max
  }
}

object Box {
  def boundingBox(points: List[Vector]): Box = {
    val xs = points.map { p => p.x }
    val ys = points.map { p => p.y }
    new Box(new Vector(xs.min, ys.max), new Vector(xs.max, ys.min))
  }

  def apply(upperLeft: Vector, lowerRight: Vector) = new Box(upperLeft, lowerRight)
  
  val stdBox = Box(Vector(-1, 1), Vector(1, -1))
}