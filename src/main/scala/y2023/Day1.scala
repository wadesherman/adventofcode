package y2023

import util.ProvidedInput

import scala.annotation.tailrec

object Day1 extends ProvidedInput with App {
  val year = 2023
  val day = 1

  val testInput: Array[String] = Array(
    "1abc2",
    "pqr3stu8vwx",
    "a1b2c3d4e5f",
    "treb7uchet"
  )

  val testPart2 = Seq(
    "two1nine",
    "eightwothree",
    "abcone2threexyz",
    "xtwone3four",
    "4nineeightseven2",
    "zoneight234",
    "7pqrstsixteen"
  )

  val part2map = Map(
    "one" -> 1,
    "1" -> 1,
    "two" -> 2,
    "2" -> 2,
    "three" -> 3,
    "3" -> 3,
    "four" -> 4,
    "4" -> 4,
    "five" -> 5,
    "5" -> 5,
    "six" -> 6,
    "6" -> 6,
    "seven" -> 7,
    "7" -> 7,
    "eight" -> 8,
    "8" -> 8,
    "nine" -> 9,
    "9" -> 9
  )

  val part1map = part2map.filter { case (key, _) => key.charAt(0).isDigit }

  def find(m: Map[String, Int], f: String => Boolean): Option[(String, Int)] =
    m.find { case (needle, _) => f(needle) }

  @tailrec
  def findFirst(s: String, m: Map[String, Int], prefix: String = ""): Int = {
    find(m, prefix.endsWith) match {
      case Some((_, integer)) => integer
      case None               => findFirst(s.substring(1), m, prefix + s.head)
    }
  }

  @tailrec
  def findLast(s: String, m: Map[String, Int], suffix: String = ""): Int = {
    find(m, suffix.startsWith) match {
      case Some((_, integer)) => integer
      case None               => findLast(s.substring(0, s.length - 1), m, s.last + suffix)
    }
  }

  def calc(s: String, m: Map[String, Int]): Int =
    findFirst(s, m) * 10 + findLast(s, m)

  def sum(data: Seq[String], map: Map[String, Int]): Int =
    data.map(calc(_, map)).sum

  assert(sum(testInput, part1map) == 142)
  val part1 = sum(providedInput, part1map)
  println(s"Part 1: $part1")

  assert(sum(testPart2, part2map) == 281)
  val part2 = sum(providedInput, part2map)
  println(s"Part 2: $part2")

}
