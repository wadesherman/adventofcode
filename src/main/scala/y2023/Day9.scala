package y2023

import util.ProvidedInput

object Day9 extends App with ProvidedInput {
  val year = 2023
  val day = 9

  val testInput = Array(
      "0 3 6 9 12 15",
      "1 3 6 10 15 21",
      "10 13 16 21 30 45",
  )

  type L = Seq[List[BigInt]]
  def parseInput(input: Array[String]): L = {
    input.map{ case s"${n}" => n.split(" ").map(BigInt(_)).toList}.toSeq
  }

  def calc(l: List[BigInt], direction: List[BigInt] => List[BigInt]): List[List[BigInt]] = {
    val next = l.sliding(2).map { case l :: r :: Nil => r - l }.toList
    if(next.forall(_ == 0)){
      direction(l) :: List(next)
    } else {
      direction(l) :: calc(next, direction)
    }
  }

  def part1(lists: L): BigInt =
    lists.map(l => calc(l, _.reverse).reverse.foldLeft(BigInt(0)){case (l, h :: _) => l + h}).sum


  def part2(lists: L): BigInt =
    lists.map(l => calc(l, _.toList).reverse.foldLeft(BigInt(0)){case (l, h :: _) => h - l}).sum

  assert(part1(parseInput(testInput)) == 114)
  println(s"Part 1: ${part1(parseInput(providedInput))}")

  assert(part2(parseInput(testInput)) == 2)
  println(s"Part 2: ${part2(parseInput(providedInput))}")

}
