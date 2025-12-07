package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day07 = Day(7) { (input, part) =>

  val ((_, startJ), splitters) =
    val rawMap = input.toLines
    val elements =
      for
        (row, i) <- rawMap.zipWithIndex
        (v, j) <- row.zipWithIndex
        value <- v match
          case 'S' => Some(true)
          case '^' => Some(false)
          case '.' => None
      yield
        (i, j) -> value
    (elements.collect { case (v, true) => v }.head, elements.collect { case (v, false) => v }.sorted)

  val splittersByJ = splitters.groupBy { case (_, j) => j }.view.mapValues(_.sorted).toMap

  val startNode = splitters.find { case (_, j) => j == startJ }.head
  val graph = splitters.flatMap { case (i, j) => Seq(-1, 1).flatMap(d => splittersByJ.getOrElse(j + d, Seq.empty).find { case (i1, _) => i < i1 }.map((i, j) -> _)) }
    .groupBy { case (from, _) => from }.view.mapValues(_.map { case (_, to) => to }.toSet).toMap.withDefaultValue(Set.empty)

  type Node = (Int, Int)

  def bfs(nodes: Set[Node], visited: Set[Node], count: Int): Int =
    val nextVisited = visited ++ nodes
    val next = nodes.flatMap(node => graph(node)).diff(nextVisited)
    if next.isEmpty then count else bfs(next, nextVisited, count + next.size)

  part(1) = bfs(Set(startNode), Set.empty, 1)

  def count(node: Node): Long =
    def countRecursive(node: Node, cache: Map[Node, Long]): (Long, Map[Node, Long]) =
      cache.get(node) match
        case Some(cached) => (cached, cache)
        case _ =>
          val neighbours = graph(node)
          val (count, newCache) = neighbours.foldLeft((1L, cache)) { case ((count, accCache), neighbour) =>
            val (addCount, newCache) = countRecursive(neighbour, accCache)
            (count + addCount, newCache)
          }
          (count, newCache + (node -> count))
    val (result, _) = countRecursive(node, Map.empty)
    result + 1

  part(2) = count(startNode)

}
