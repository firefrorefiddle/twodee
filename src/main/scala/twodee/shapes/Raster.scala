package twodee.shapes

import twodee.geometry.Vector

/**
 * A raster is a mapping from a viewport in Double coordinates to
 * a typical screen raster as in console output of lines and columns
 * or in an HTML canvas. It is defined by the side length and the
 * viewPort.
 *
 * TODO: allow non-square rasters (easy).
 */
case class Raster (sideLength : Int, viewPort : Box) {

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
case class RasterPoint(x : Int, y: Int) {
  override def toString : String = {
    "<"+x+","+y+">"
  }
}
