package y2024

import util.ProvidedInput
import util.Utils.memoize

object Day11 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 11
  override val testInput: Array[String] = Array(
    "125 17"
  )

  def parseInput(input: Array[String]): List[Long] = input.head.split(" ").map(_.toLong).toList

  lazy val applyRules: Long => (Long, Option[Long]) = memoize { (n: Long) =>
    if (n == 0) {
      (1, None)
    } else if (n.toString.length % 2 == 0) {
      val (l, r) = n.toString.splitAt(n.toString.length / 2)
      (l.toLong, Some(r.toLong))
    } else {
      (n * 2024, None)
    }
  }

  lazy val blink: ((Int, Long)) => Long = memoize[(Int, Long), Long] { case (n, rock) => {
    if(n == 0){
      1
    } else {
      val (l, r) = applyRules(rock)
      val foo: Long = blink(n - 1, l)
      val bar: Long = r.map(blink(n - 1, _)).getOrElse(0)
      (foo) + (bar)
    }
  }}

  def countRocks(rocks: List[Long], blinks: Int): Long = rocks.map(r => blink(blinks, r)).sum

  assert(countRocks(parseInput(testInput), 25) == 55312)
  val part1 = run { countRocks(parseInput(providedInput), 25) }
  println(s"Part 1: $part1")

  val part2 = run { countRocks(parseInput(providedInput), 75) }
  print(s"Part 2: $part2")
}
