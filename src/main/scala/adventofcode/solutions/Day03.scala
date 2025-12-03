package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day03 = Day(3) { (input, part) =>

  val batteries = input.toLines.map(_.map(_.asDigit).toIndexedSeq)

  def joltage(battery: IndexedSeq[Int], i: Int, size: Int): Option[Seq[Int]] =
    if size > 0 then
      val digits = battery.zipWithIndex.drop(i).dropRight(size - 1).sortBy((d, i) => (-d, i))
      digits.view.flatMap((d, i) => joltage(battery, i + 1, size - 1).map(d +: _)).headOption
    else
      Some(Seq.empty)

  def joltages(size: Int): Long = batteries.flatMap(battery => joltage(battery, 0, size).map(_.mkString.toLong)).sum

  part(1) = joltages(2)

  part(2) = joltages(12)

}
