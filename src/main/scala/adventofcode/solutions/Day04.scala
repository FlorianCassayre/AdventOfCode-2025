package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day04 = Day(4) { (input, part) =>

  val grid = input.toLines.map(_.map {
    case '@' => true
    case _ => false
  })

  val range = -1 to 1
  val directions =
    for
      i <- range
      j <- range
      if i.abs + j.abs > 0
    yield (i, j)

  def accessible(grid: IndexedSeq[IndexedSeq[Boolean]]): Seq[(Int, Int)] =
    for
      i0 <- grid.indices
      j0 <- grid(i0).indices
      if grid(i0)(j0)
      if directions
        .view
        .map((di, dj) => (i0 + di, j0 + dj))
        .filter((i, j) => grid.indices.contains(i) && grid(i).indices.contains(j))
        .count((i, j) => grid(i)(j)) < 4
    yield (i0, j0)

  part(1) = accessible(grid).size

  def remove(grid: IndexedSeq[IndexedSeq[Boolean]], total: Int): Int =
    val positions = accessible(grid).toSet
    if positions.nonEmpty then
      remove(grid.zipWithIndex.map((row, i) => row.zipWithIndex.map((v, j) => v && !positions.contains((i, j)))), total + positions.size)
    else
      total

  part(2) = remove(grid, 0)

}
