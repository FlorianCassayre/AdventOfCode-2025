package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day02 = Day(2) { (input, part) =>

  val ranges = input.trim.split(",").map { case s"$a-$b" => a.toLong to b.toLong }

  def count(allowed: Int => Boolean): Long =
    ranges.flatten.filter { n =>
      val s = s"$n"
      (1 until s.length).exists { size =>
        val range = 0 until s.length by size
        allowed(range.size) && range.map(i => s.slice(i, i + size)).distinct.sizeIs == 1
      }
    }.sum

  part(1) = count(_ == 2)

  part(2) = count(_ => true)

}
