package twodee.shapes

import twodee.geometry._
import twodee.transform._

trait Shape extends Transformable[Shape] {
  def inside(p: Vector): Boolean
  def boundingPoly: List[Vector]
  
  def +(s: Shape) = Union(this, s)
  def -(s: Shape) = Difference(this, s)
  def &(s: Shape) = Intersection(this, s)
}

case class Union(s1: Shape, s2: Shape) extends Shape {
  // FIXME: vector order; however, it's no problem for the bounding box...
  def boundingPoly = s1.boundingPoly ++ s2.boundingPoly
  def inside(p: Vector) = s1.inside(p) || s2.inside(p)
  // FIXME: calculate from union poly
  val centre = (s1.centre + s2.centre) / 2
  def transform(t: Transformation) = Union(s1.transform(t), s2.transform(t))
}

case class Difference(s1: Shape, s2: Shape) extends Shape {
  // FIXME: cut poly
  def boundingPoly = s1.boundingPoly
  def inside(p: Vector) = s1.inside(p) && !s2.inside(p)
  // FIXME: calculate from cut poly
  val centre = s1.centre
  def transform(t: Transformation) = Difference(s1.transform(t), s2.transform(t))
}

case class Intersection(s1: Shape, s2: Shape) extends Shape {
  
  // FIXME: cut poly
  def boundingPoly = s1.boundingPoly ++ s2.boundingPoly
  def inside(p: Vector) = s1.inside(p) && s2.inside(p)
  // FIXME: calculate from cut poly  
  val centre = (s1.centre + s2.centre) / 2
  def transform(t: Transformation) = Intersection(s1.transform(t), s2.transform(t))
}
