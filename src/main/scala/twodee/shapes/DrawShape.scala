package twodee.shapes

object DrawShape {
  def apply(s: Shape, r: Raster): String = {
    var sb = new StringBuilder
    for (j <- List.range(0, r.sideLength)) {
      for (i <- List.range(0, r.sideLength)) {
        if (s.inside(r.toPoint(new RasterPoint(i, j)))) {
          sb.append("##")
        } else {
          sb.append("..")
        }
      }
      sb.append("\n")
    }
    sb.toString()
  }
}
