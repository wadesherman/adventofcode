package y2024

import util.ProvidedInput

object Day1 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 1
  override val testInput: Array[String] = Array(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3"
  )

  def splitInput(input: Array[String]) = input
    .map(_.split("\\s+")
      .map(_.toInt)
      .toList match {
        case l :: r :: Nil => (l, r)
      }
    )
    .unzip


  def listDifference(lists: (Array[Int], Array[Int])): Int = {
    val (l, r) = lists
    l.sorted
      .zip(r.sorted)
      .map { case (l, r) => (r - l).abs }
      .sum
  }

  def listSimilarity(lists: (Array[Int], Array[Int])): Int = {
    val (l, r) = lists
    val counts: Map[Int, Int] = r.foldLeft(Map.empty[Int, Int]) { (acc, value) =>
      val runningCount = acc.getOrElse(value, 0)
      acc + (value -> (runningCount + 1))
    }
    l.foldLeft(0){ (acc, value) =>
      val totalCount = counts.getOrElse(value, 0)
      acc + (value * totalCount)
    }
  }

  assert(listDifference(splitInput(testInput)) == 11)
  val part1 = listDifference(splitInput(providedInput))
  println(s"Part 1: $part1")

  assert(listSimilarity(splitInput(testInput)) == 31)
  val part2 = listSimilarity(splitInput(providedInput))
  println(s"Part 2: $part2")


}
