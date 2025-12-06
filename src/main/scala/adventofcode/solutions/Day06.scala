package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day06 = Day(6) { (input, part) =>

  val parts = input.toLines

  val (operations, operationIndices) = parts.last.zipWithIndex.flatMap { (c, i) =>
    val op: Option[(Long, Long) => Long] = c match
      case '+' => Some(_ + _)
      case '*' => Some(_ * _)
      case _ => None
    op.map(_ -> i)
  }.unzip
  val ranges = operationIndices.zip(operationIndices.tail.map(_ - 1) :+ parts.map(_.length).max)

  val numbers = parts.init.map(s => ranges.map((a, b) => s.slice(a, b))).transpose

  val templates = operations.zip(numbers)

  def compute(f: IndexedSeq[String] => IndexedSeq[IndexedSeq[Char]]): Long =
    templates.map((op, ns) => f(ns).map(_.mkString.trim.toLong).reduce(op)).sum

  part(1) = compute(_.map(identity))

  part(2) = compute(_.transpose)

}
