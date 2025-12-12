package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day12 = Day(12) { (input, part) =>

  case class Region(width: Int, length: Int, quantities: IndexedSeq[Int])

  val parts = input.split(lineSeparator * 2)
  val shapes = parts.init.map(_.split(lineSeparator).toSeq).map { case s"$index:" +: shape => index.toInt -> shape.map(_.map {
    case '#' => true
    case '.' => false
  }).toIndexedSeq }.toMap
  val regions = parts.last.split(lineSeparator).map {
    case s"${width}x${length}: $quantities" => Region(width.toInt, length.toInt, quantities.split(" ").map(_.toInt).toIndexedSeq)
  }

  def isPossible(region: Region): Boolean =
    val maxSize = shapes.values.map(_.size).max.max(shapes.values.flatMap(_.map(_.size)).max)
    region.quantities.sum <= (region.width / maxSize) * (region.length / maxSize)

  def isImpossible(region: Region): Boolean =
    region.width * region.length < region.quantities.zip(region.quantities.indices.map(shapes).map(_.map(_.count(identity)).sum)).map(_ * _).sum

  part(1) = regions.count { region =>
    val (possible, impossible) = (isPossible(region), isImpossible(region))
    assert(possible != impossible)
    possible
  }

  part(2) = ""

}
