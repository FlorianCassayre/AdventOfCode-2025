package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day05 = Day(5) { (input, part) =>

  case class Range(low: Long, high: Long):
    def size: Long = high - low + 1
    def contains(v: Long): Boolean = low <= v && v <= high

  val (fresh, available) = input.split(lineSeparator * 2).map(_.split(lineSeparator)).toSeq match
    case Seq(first, second) => (first.map { case s"$a-$b" => Range(a.toLong, b.toLong) }, second.map(_.toLong))

  part(1) = available.count(ingredient => fresh.exists(_.contains(ingredient)))

  val bounds = fresh.flatMap(r => Seq(r.low, r.high)).flatMap(v => Seq(v, v + 1)).sorted.distinct
  val disjoint = bounds.zip(bounds.tail.map(_ - 1)).map(Range.apply)

  val valid = disjoint.filter(r => fresh.exists(_.contains(r.low)))

  part(2) = valid.map(_.size).sum

}
