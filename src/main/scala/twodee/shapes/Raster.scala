package twodee.shapes

import twodee.geometry.Vector

class Raster (sideLengthC : Int, viewPortC : Box) {
    val sideLength = sideLengthC
    val viewPort = viewPortC

    def fromPoint(p : Vector) : RasterPoint = { 
      new RasterPoint(
          ((p.x - viewPort.upperLeft.x) / viewPort.maxDim * sideLength).toInt,
          ((p.y - viewPort.upperLeft.y) / viewPort.maxDim * -sideLength).toInt)          
    }
    
    def toPoint(p : RasterPoint) : Vector = { 
      new Vector(          
          ((p.x + 0.5) * viewPort.maxDim / sideLength)  + viewPort.upperLeft.x,
          ((p.y + 0.5) * viewPort.maxDim / -sideLength) + viewPort.upperLeft.y)          
    }    
}

/**
 * @author mhartl
 */
class RasterPoint(xc : Int, yc: Int) {
  val x : Int = xc
  val y : Int = yc
  override def toString : String = {
    "<"+x+","+y+">"
  }
}
