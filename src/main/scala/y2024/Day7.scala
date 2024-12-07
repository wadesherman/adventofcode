package y2024

import util.ProvidedInput

object Day7 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 7
  override val testInput: Array[String] = Array(
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20"
  )

  type Equation = (Long, List[Long])
  type Equations = Array[Equation]

  def parseInput(input: Array[String]): Equations = {
    input.map { case s"$d: $w" =>
      d.toLong -> w.split(" ").map(_.toLong).toList
    }
  }

  type Op = (Long, Long) => Long
  def plus: Op = { case (l, r) => l + r }
  def times: Op = { case (l, r) => l * r }
  def concat: Op = { case (l, r) => (l.toString + r.toString).toLong }

  def canBeSolved(equation: Equation, operations: Seq[Op]): Boolean = {
    def validate(t: Long, v: Long, remaining: List[Long], ops: Seq[Op]): Boolean = {
      if (remaining.isEmpty) {
        if (t == v) true else false
      } else if (v > t) {
        false
      } else {
        ops.exists(op => validate(t, op(v, remaining.head), remaining.tail, ops))
      }
    }

    val (result, values) = equation
    validate(result, values.head, values.tail, operations)
  }

  def calbrationResult(equations: Equations, operations: Seq[Op]): Long = {
    equations.filter(canBeSolved(_, operations)).map(_._1).sum
  }

  assert(calbrationResult(parseInput(testInput), Seq(plus, times)) == 3749)
  val part1 = run { calbrationResult(parseInput(providedInput), Seq(plus, times)) }
  println(s"Part 1: $part1")

  assert(calbrationResult(parseInput(testInput), Seq(plus, times, concat)) == 11387)
  val part2 = run { calbrationResult(parseInput(providedInput), Seq(plus, times, concat)) }
  println(s"Part 2: $part2")
}
