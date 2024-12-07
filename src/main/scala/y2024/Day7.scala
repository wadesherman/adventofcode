package y2024

import util.ProvidedInput

import scala.language.implicitConversions

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
    "292: 11 6 16 20",
  )

  type Equation = (Long, List[Long])
  type Equations = Array[Equation]

  def parseInput(input: Array[String]): Equations = {
    input.map {
      case s"$d: $w" => d.toLong -> w.split(" ").map(_.toLong).toList
    }
  }

 implicit class FancyLong(v: Long) {
    implicit def ||(l: Long): Long = (v.toString + l.toString).toLong
  }

  def canBeSolved(equation: Equation): Boolean = {
    def validate(t: Long, v: Long, remaining: List[Long]): Boolean = {
      if(remaining.isEmpty) {
        if(t == v) true else false
      } else if (v > t) {
        false
      } else {
        validate(t, v + remaining.head, remaining.tail) || validate(t, v * remaining.head, remaining.tail)
      }
    }

    val (result, values) = equation
    validate(result, values.head, values.tail)
  }


  def canBeSolved2(equation: Equation): Boolean = {
    def validate(t: Long, v: Long, remaining: List[Long]): Boolean = {
      if(remaining.isEmpty) {
        if(t == v) true else false
      } else if (v > t) {
        false
      } else {
        validate(t, v + remaining.head, remaining.tail) ||
          validate(t, v * remaining.head, remaining.tail) ||
          validate(t, (v.toString + remaining.head.toString).toLong, remaining.tail)
      }
    }
    val (result, values) = equation
    validate(result, values.head, values.tail)
  }

  def calbrationResult(equations: Equations): Long = {
    equations.filter(canBeSolved).map(_._1).sum
  }

  def calbrationResult2(equations: Equations): Long = {
    equations.filter(canBeSolved2).map(_._1).sum
  }

  assert(calbrationResult(parseInput(testInput)) == 3749)
  val part1 = run {calbrationResult(parseInput(providedInput)) }
  println(s"Part 1: $part1")

  assert(calbrationResult2(parseInput(testInput)) == 11387)
  val part2 = run {calbrationResult2(parseInput(providedInput)) }
  println(s"Part 2: $part2")
}
