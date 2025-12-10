package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.immutable.BitSet

@main def Day10 = Day(10) { (input, part) =>

  case class Machine(size: Int, indicator: BitSet, wiring: Seq[BitSet], requirement: Seq[Int])

  val machines = input.toLines.map { case s"[$indicator] $wiring {$requirement}" =>
    val indicatorIndices = indicator.map {
      case '#' => true
      case '.' => false
    }
    Machine(
      indicatorIndices.size,
      BitSet(indicatorIndices.zipWithIndex.flatMap((b, i) => if b then Some(i) else None)*),
      wiring.split(" ").map { case s"($s)" => BitSet(s.split(",").map(_.toInt)*) },
      requirement.split(",").map(_.toInt)
    )
  }

  part(1) = machines.flatMap(machine =>
    (0 to machine.size).view.flatMap(i =>
      machine.wiring.combinations(i)
        .map(_.foldLeft(BitSet())(_ ^ _)).filter(_ == machine.indicator).map(_ => i)
    ).take(1)
  ).sum

  import optimus.optimization._
  import optimus.optimization.enums.SolverLib
  import optimus.optimization.model.MPIntVar
  import optimus.algebra.{Expression, Int2Const}

  def optimizeILP(machine: Machine): Int =
    given MPModel(SolverLib.oJSolver)
    val max = machine.requirement.max
    val vars = machine.wiring.indices.map(i => MPIntVar(0 to max))
    val wiringSet = machine.wiring.map(_.toSet)
    (0 until machine.size).map { i =>
      val lhs = wiringSet.zipWithIndex.collect { case (set, j) if set.contains(i) => j }.foldLeft[Expression](0)((acc, j) => vars(j) + acc)
      add(lhs := (machine.requirement(i): Expression))
    }
    minimize(vars.reduce(_ + _))
    start()
    val result = vars.map(_.value.get.round.toInt)
    release()
    result.sum

  part(2) = machines.map(optimizeILP).sum

}
