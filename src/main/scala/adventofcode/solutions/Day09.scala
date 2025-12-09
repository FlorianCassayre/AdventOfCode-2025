package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day09 = Day(9) { (input, part) =>

  case class Vec(x: Int, y: Int):
    def area(that: Vec): Long = ((x - that.x).toLong.abs + 1) * ((y - that.y).abs + 1)
    def min(that: Vec): Vec = Vec(x.min(that.x), y.min(that.y))
    def max(that: Vec): Vec = Vec(x.max(that.x), y.max(that.y))
    inline def +(that: Vec): Vec = Vec(x + that.x, y + that.y)
    def rim(that: Vec): Seq[Vec] =
      val (minV, maxV) = (min(that) + Vec(1, 1), max(that) + Vec(-1, -1))
      Seq(Vec(minV.x, minV.y), Vec(maxV.x, minV.y), Vec(minV.x, maxV.y), Vec(maxV.x, maxV.y)) ++
        (minV.x + 1 until maxV.x).flatMap(x => Seq(Vec(x, minV.y), Vec(x, maxV.y))) ++
          (minV.y + 1 until maxV.y).flatMap(y => Seq(Vec(minV.x, y), Vec(maxV.x, y)))

  val tiles = input.toLines.map { case s"$x,$y" => Vec(x.toInt, y.toInt) }

  val rectangles =
    for
      (a, i) <- tiles.zipWithIndex.view
      b <- tiles.view.drop(i + 1)
    yield a.area(b)

  part(1) = rectangles.max

  def compress(property: Vec => Int): Map[Int, Int] =
    tiles.map(property).sorted.distinct.zipWithIndex.toMap

  val (compressedX, compressedY) = (compress(_.x), compress(_.y))

  val compressedTiles = tiles.map(vec => Vec(compressedX(vec.x), compressedY(vec.y)))
  val inverseCompression = compressedTiles.zip(tiles).toMap

  val bordered = compressedTiles.zip(compressedTiles.tail :+ compressedTiles.head).foldLeft(Set.empty[Vec]) { case (set, (a, b)) =>
    val (min, max) = (a.min(b), a.max(b))
    val size = (max.x - min.x).max(max.y - min.y)
    set ++ (0 to size).map(i => Vec((max.x - min.x).sign * i + min.x, (max.y - min.y).sign * i + min.y))
  }

  val range = -1 to 1
  val directions =
    for
      x <- range
      y <- range
      if x.abs + y.abs == 1
    yield
      Vec(x, y)

  val rectanglesCompressed =
    for
      (a, i) <- compressedTiles.zipWithIndex.view
      b <- compressedTiles.view.drop(i + 1)
      rim = a.rim(b)
      if !rim.exists(bordered.contains)
    yield inverseCompression(a).area(inverseCompression(b))

  part(2) = rectanglesCompressed.max

}
