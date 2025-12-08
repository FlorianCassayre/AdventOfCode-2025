package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day08 = Day(8) { (input, part) =>

  object UnionFind:

    opaque type Internal[U] = (Map[U, U], Map[U, Int], Int)

    def initial[U]: Internal[U] =
      (Map.empty, Map.empty, 0)

    def find[U](state: Internal[U], x: U): (Internal[U], U) =
      val (forest, ranks, comps) = state
      val (forest2, ranks2, comps2) =
        if forest.contains(x) then state
        else (forest + (x -> x), ranks, comps + 1)
      forest2.get(x) match
        case Some(y) if x != y =>
          val (stateAfterRecursiveFind, root) = find((forest2, ranks2, comps2), y)
          val (f3, r3, c3) = stateAfterRecursiveFind
          val compressedForest = f3 + (x -> root)
          ((compressedForest, r3, c3), root)
        case _ =>
          ((forest2, ranks2, comps2), x)

    def union[U](state: Internal[U], x: U, y: U): Internal[U] =
      val (stateAfterFindX, xr) = find(state, x)
      val (stateAfterFindY, yr) = find(stateAfterFindX, y)
      val (forest, ranks, comps) = stateAfterFindY
      if xr != yr then
        val rx = ranks.getOrElse(xr, 0)
        val ry = ranks.getOrElse(yr, 0)
        if rx < ry then
          (forest + (xr -> yr), ranks, comps - 1)
        else
          val newForest = forest + (yr -> xr)
          if rx == ry then
            (newForest, ranks + (xr -> (rx + 1)), comps - 1)
          else
            (newForest, ranks, comps - 1)
      else
        stateAfterFindY

    def components[U](state: Internal[U]): Int = state._3

  case class Vec(x: Int, y: Int, z: Int):
    private def sq(v: Long): Long = v * v
    def distanceSq(that: Vec): Long = sq(x - that.x) + sq(y - that.y) + sq(z - that.z)

  val positions = input.toLines.map {
    case s"$x,$y,$z" => Vec(x.toInt, y.toInt, z.toInt)
  }

  val pairs =
    for
      (a, i) <- positions.zipWithIndex
      b <- positions.drop(i + 1)
    yield (a, b)

  val ordered = pairs.sortBy((a, b) => a.distanceSq(b))

  val result = ordered.take(1000).foldLeft(UnionFind.initial[Vec]) { case (state, (a, b)) => UnionFind.union(state, a, b) }

  val (_, representatives) = positions.foldLeft((result, Seq.empty[Vec])) { case ((state, seq), e) =>
    val (nextState, v) = UnionFind.find(state, e)
    (nextState, seq :+ v)
  }

  val clusters = representatives.groupBy(identity).values.map(_.size).toSeq.sortBy(-_)

  part(1) = clusters.take(3).product

  def findLast(state: UnionFind.Internal[Vec], ordered: Seq[(Vec, Vec)]): (Vec, Vec) = ordered match
    case (a, b) +: tail =>
      val nextState = UnionFind.union(state, a, b)
      if UnionFind.components(nextState) > 1 then
        findLast(nextState, tail)
      else
        (a, b)

  lazy val (lastA, lastB) = findLast(positions.foldLeft(UnionFind.initial[Vec])((state, e) => UnionFind.union(state, e, e)), ordered)

  part(2) = lastA.x.toLong * lastB.x.toLong

}
