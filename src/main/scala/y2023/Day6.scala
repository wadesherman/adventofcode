package y2023

import util.ProvidedInput

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode

object Day6 extends App with ProvidedInput {
  val year = 2023
  val day = 6
  val testInput = Array(
    "Time:      7  15   30",
    "Distance:  9  40  200"
  )

  def parseInput(input: Array[String]): Seq[(Int, Int)] = {
    val td: Seq[Seq[Int]] = input
      .map { case s"${_}: ${data}" => data }
      .map(_.stripLeading().stripTrailing().replaceAll("\\s+", " "))
      .map(_.split(" ").map(_.toInt).toSeq)

    (td(0).zip(td(1)))
  }

  def parseInputPart2(input: Array[String]): (BigInt, BigInt) = {
    val td: Array[BigInt] = input
      .map { case s"${_}: ${data}" => data }
      .map(_.replaceAll("\\s+", ""))
      .map(s => BigInt(s))

    (td(0), (td(1)))
  }

  def part1(data: Seq[(Int, Int)]) = data
    .map {
      case (timeLimit, distanceRecord) => {

        (0 to timeLimit)
          .map(t => {
            val speed = t
            val runTime = timeLimit - t
            speed * runTime
          })
          .filter(_ > distanceRecord)
      }
    }
    .map(_.length)
    .product

  def part2BruteForce(data: (BigInt, BigInt)): BigInt = {
    val (timeLimit, distanceRecord) = data

    val starts = LazyList.range(BigInt(0), timeLimit)
    starts.foldLeft(BigInt(0)) {
      case (z, t) => {
        val speed = t
        val runTime = timeLimit - t
        val d = speed * runTime
        if (d > distanceRecord) {
          z + 1
        } else {
          z
        }
      }
    }
  }

  def sqrt(number: BigInt): BigDecimal = {
    val approximateRoot = search(0, number, 0, number, { n => n * n })
    BigDecimal(approximateRoot)
  }

  def part2QuadraticRoot(data: (BigInt, BigInt)): BigDecimal = {
    val (timeLimit, distanceRecord) = data
    val a = -1
    val b = timeLimit
    val c = -distanceRecord

    val root = sqrt((b * b) - (4 * a * c))
    val l = (BigDecimal(-b) + root) / BigDecimal(2 * a)
    val r = (BigDecimal(-b) - root) / BigDecimal(2 * a)

    (r.setScale(0, RoundingMode.DOWN) - l.setScale(0, RoundingMode.UP)) + 1
  }

  @tailrec
  def search(
      n: BigInt,
      t: BigInt,
      min: BigInt,
      max: BigInt,
      d: BigInt => BigInt
  ): BigInt = {
    val l = d(n)
    val r = d(n + 1)
    if (l == t || r == t) {
      t
    } else if (l < t && r > t) {
      n
    } else if (r < t) {
      //search to the right
      search((n + max) / 2, t, n, max, d)
    } else {
      //search to the left
      search((n + min) / 2, t, min, n, d)
    }
  }

  def part2Binary(data: (BigInt, BigInt)): BigInt = {
    val (timeLimit, distanceRecord) = data
    val d: BigInt => BigInt = { x => (timeLimit * x) - (x * x) }

    val mid = timeLimit / 2
    val leftEdge = search(mid, distanceRecord, 0, mid, d)
    timeLimit - ((leftEdge + 1) * 2) + 1

  }

  assert(part1(parseInput(testInput)) == 288)
  assert(part2QuadraticRoot(parseInputPart2(testInput)) == 71503)
  assert(part2BruteForce(parseInputPart2(testInput)) == 71503)
  assert(part2Binary(parseInputPart2(testInput)) == 71503)

  println(s"Part 1: ${part1(parseInput(providedInput))}")

  val t1 = System.currentTimeMillis()

  val part2B = part2Binary(parseInputPart2(providedInput))
  val t2 = System.currentTimeMillis()

  println(s"Part 2: ${part2B} ${t2 - t1}ms")

  val part2Result = part2QuadraticRoot(parseInputPart2(providedInput))
  val t3 = System.currentTimeMillis()

  println(s"Part 2 Quadratic: ${part2Result} ${t3 - t2}ms")

  val part2bf = part2BruteForce(parseInputPart2(providedInput))
  val t4 = System.currentTimeMillis()

  println(s"Part 2 BruteForce: ${part2bf} ${t4 - t3}ms")

}
