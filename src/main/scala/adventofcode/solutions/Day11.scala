package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day11 = Day(11) { (input, part) =>

  val graph = input.toLines.map { case s"$from: $to" =>
    from -> to.split(" ").toSet
  }.toMap

  val (start, end) = ("you", "out")

  def count1(cache: Map[String, Long], from: String): (Map[String, Long], Long) = {
    if from == end then
      (cache, 1)
    else if cache.contains(from) then
      (cache, cache(from))
    else
      val (newCache, result) = graph(from).foldLeft((cache, 0L)) { case ((accCache, total), neighbour) =>
        val (newCache, addTotal) = count1(accCache, neighbour)
        (newCache, total + addTotal)
      }
      (newCache + (from -> result), result)
  }

  val (_, total1) = count1(Map.empty, start)

  part(1) = total1

  val start2 = "svr"
  val mustVisit = Set("dac", "fft")

  def count2(cache: Map[(String, Set[String]), Long], from: String, visited: Set[String]): (Map[(String, Set[String]), Long], Long) = {
    val newVisited = visited ++ Set(from).intersect(mustVisit)
    if from == end then
      (cache, if newVisited == mustVisit then 1 else 0)
    else if cache.contains((from, newVisited)) then
      (cache, cache((from, newVisited)))
    else
      val (newCache, result) = graph(from).foldLeft((cache, 0L)) { case ((accCache, total), neighbour) =>
        val (newCache, addTotal) = count2(accCache, neighbour, newVisited)
        (newCache, total + addTotal)
      }
      (newCache + ((from, newVisited) -> result), result)
  }

  val (_, total2) = count2(Map.empty, start2, Set.empty)

  part(2) = total2

}
