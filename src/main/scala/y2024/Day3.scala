package y2024

import util.ProvidedInput

object Day3 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 3
  override val testInput: Array[String] = Array("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")


  def parseInput(input: Array[String]): String = input.mkString

  def parseInputPart2(input: Array[String]): String = {
    def f(section: String): List[String] = {
     section.split("do()").toList match {
        case ::(_, next) => next
        case Nil => Nil
     }
    }

    (parseInput(input)
    .split("don't()").toList match {
    case ::(head, next) => head :: next.flatMap(f(_))
    case Nil => Nil
  }).mkString
  }

  val regex = """(mul\((\d+),(\d+)\))""".r

  def mul(input: String): Int = {
    regex.findAllMatchIn(input).foldLeft(0){ case (acc, m) => {
      m.subgroups match {
        case List(_, l, r) => acc + l.toInt * r.toInt
      }
    }}
  }

  assert(mul(parseInput(testInput)) == 161)
  val part1 = mul(parseInput(providedInput))
  println(s"Part 1: $part1")

  assert(mul(parseInputPart2(testInput)) == 48)
  val part2 = mul(parseInputPart2(providedInput))
  println(s"Part 2: $part2")


}
