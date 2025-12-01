package adventofcode.solutions

import adventofcode.Definitions.*
import java.lang.Math.floorDiv

@main def Day01 = Day(1) { (input, part) =>

  val (m, start) = (100, 50)

  val instructions = input.toLines.map {
    case s"L$n" => n.toInt
    case s"R$n" => -n.toInt
  }

  val (_, zeros1, zeros2) = instructions.foldLeft((50, 0, 0)) {
    case ((current, count1, count2), move) =>
      val next = current + move
      val endings = if next % m == 0 then 1 else 0
      val crossings =
        if move >= 0 then
          floorDiv(next, m) - floorDiv(current, m)
        else
          floorDiv(current - 1, m) - floorDiv(next - 1, m)
      (next, count1 + endings, count2 + crossings)
  }

  part(1) = zeros1

  part(2) = zeros2
}
