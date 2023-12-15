package y2023

import util.ProvidedInput
import util.Utils.rotateRight

import scala.annotation.tailrec

object Day13 extends App with ProvidedInput {
  val year = 2023
  val day = 13
  val testInput: Array[String] = Array(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  )

  case class Puzzle(
      data: Array[Array[Char]]
  ) {
    def h: Int = data.length
    def w: Int = data.head.length
    def normal: Array[String] = data.map(_.mkString)
    def rotated: Array[String] = rotateRight(data).map(_.mkString)
  }

  type CandidateFunction = Array[String] => Array[Array[String]]
  type ScoringFunction = (Array[String], Int) => Int

  def parseInput(input: Seq[String]): Array[Puzzle] = {
    input
      .mkString("-")
      .split("--")
      .map(_.split("-"))
      .map(p => Puzzle(p.map(_.toCharArray)))
  }

  def cleanCandidates: CandidateFunction = { r =>
    r.zipWithIndex
      .filter { case (l, _) => l == r.head }
      .map { case (_, i) => r.slice(0, i + 1) }
  }

  def smudgedCandidates: CandidateFunction = { r =>
    val indexedHead = r.head.zipWithIndex
    val smudgeOnEnd = r.zipWithIndex
      .filter { case (l, _) => l.zipWithIndex.intersect(indexedHead).length == indexedHead.length - 1 }
      .map { case (_, i) => r.slice(0, i + 1) }

    cleanCandidates(r) ++ smudgeOnEnd
  }

  @tailrec
  def smudgedPalindromeLength(test: Array[String], smudgeAvailable: Boolean, c: Option[Int] = None): Option[Int] = {

    if (test.length < 2) {
      None
    } else {

      val indexedTest = test.map(_.zipWithIndex)
      val h = indexedTest.length
      val w = indexedTest.head.length

      if (test.length == 2 && test(0) == test(1) && smudgeAvailable) {
        None
      } else if (test.length == 2 && test(0) == test(1) && !smudgeAvailable) {
        Some(c.getOrElse(0) + 2)
      } else if (test.length == 2 && smudgeAvailable && indexedTest(0).intersect(indexedTest(h - 1)).length == w - 1) {
        Some(c.getOrElse(0) + 2)
      } else if (test(0) == test(h - 1)) {
        smudgedPalindromeLength(test.slice(1, h - 1), smudgeAvailable, Some(c.getOrElse(0) + 2))
      } else if (smudgeAvailable && indexedTest(0).intersect(indexedTest(h - 1)).length == w - 1) {
        smudgedPalindromeLength(test.slice(1, h - 1), false, Some(c.getOrElse(0) + 2))
      } else {
        None
      }
    }
  }

  def potentialPalindromes(
      r: Array[String],
      candidates: CandidateFunction,
      f: ScoringFunction,
      smudgeAvailable: Boolean
  ): Option[Int] = {
    candidates(r)
      .flatMap(c => smudgedPalindromeLength(c, smudgeAvailable, None).map(f(r, _)))
      .sorted
      .reverse
      .headOption
  }

  val halfLength: ScoringFunction = { case (_, l) => l / 2 }
  val halfLengthPlusRemainder: ScoringFunction = { case (a, l) => (l / 2) + (a.length - l) }

  def score(input: Array[Puzzle], c: CandidateFunction, s: Boolean): Int = {
    val f = potentialPalindromes(_, c, _, s)
    input
      .flatMap(p => {
        val nn = f(p.normal, halfLength).map(_ * 100)
        val nr = f(p.normal.reverse, halfLengthPlusRemainder).map(_ * 100)
        val rn = f(p.rotated, halfLength)
        val rr = f(p.rotated.reverse, halfLengthPlusRemainder)
        nn orElse nr orElse rn orElse rr
      })
      .sum
  }

  def part1(input: Array[Puzzle]): Int = score(input, cleanCandidates, false)
  def part2(input: Array[Puzzle]): Int = score(input, smudgedCandidates, true)

  assert(part1(parseInput(testInput)) == 405)
  assert(part2(parseInput(testInput)) == 400)
  println(s"Part 1: ${part1(parseInput(providedInput))}")
  println(s"Part 2: ${part2(parseInput(providedInput))}")

}
