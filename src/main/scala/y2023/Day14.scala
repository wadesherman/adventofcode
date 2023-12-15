package y2023

import util.Utils.{rotateLeft, rotateRight}
import util.{ProvidedInput, Utils}

object Day14 extends App with ProvidedInput {
  val year = 2023
  val day = 14
  val testInput = Array(
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#...."
  )

  type AS = Array[String]
  type AC = Array[Char]
  type AAC = Array[Array[Char]]
  type Cycle = (Int, Int, Seq[Int])

  def parseInput(input: AS): AAC = {
    input.map(_.toArray)
  }

  def tiltLeft(str: AC): AC = {
    str.mkString
      .replaceAll("#", "-#-")
      .split("-")
      .map(s => s.sorted.reverse)
      .reduce(_ ++ _)
      .toArray
  }

  def scoreField(field: AAC): Int = {
    field
      .map(r =>
        {
          val w = r.length
          val foo: Seq[Int] = r.zipWithIndex
            .collect { case (c, i) if c == 'O' => w - i }
          foo
        }.sum
      )
  }.sum

  def cycleLength(seq: Seq[Int]): Option[Int] = {
    if (seq.length % 2 == 0) {
      None
    } else {
      val (r, l) = seq.dropRight(1).splitAt(seq.length / 2)
      if (r == l) {
        Some(r.length)
      } else {
        None
      }
    }
  }

  // Left is invalid.
  // Right(None) is a not cycle but not invalid yet
  // Right(Some(l)) is a valid cycle of length l
  def checkCycle(seq: Seq[Int]): Either[String, Option[Int]] = {
    if (seq.count(_ == seq.head) == 3) {
      cycleLength(seq) match {
        case Some(length) => Right(Some(length))
        case None         => Left("not a cycle")
      }
    } else {
      Right(None)
    }
  }

  lazy val score: AAC => Int = Utils.memoize { case arr =>
    scoreField(arr)
  }

  lazy val rotateAndTilt: AAC => AAC = Utils.memoize { case arr =>
    rotateRight(arr.map(tiltLeft))
  }

  //Iterate until we detect a cycle in the output.
  def nRotations(start: AAC, n: BigInt): Int = {
    val rotateForever = LazyList.iterate((start, Seq(Seq.empty[Int]), 0, Option.empty[Cycle])) {
      case (platform, potentialCycles, i, none) => {
        val rotated = rotateAndTilt(platform)
        val s = score(rotated)
        val newCycles: Seq[(Seq[Int], Either[String, Option[Int]])] = (Seq.empty +: potentialCycles)
          .map { c =>
            {
              val newC = c :+ s
              (newC, checkCycle(newC))
            }
          }
          .filter { case (_, check) => check.isRight }

        val exitCycle = newCycles.collectFirst { case (c, Right(Some(l))) => (l, c) }

        exitCycle match {
          case Some(_) => (rotated, Nil, i, exitCycle.map(l => (i - (l._1 * 2), l._1, l._2)))
          case None    => (rotated, newCycles.map(_._1), i + 1, none)
        }
      }
    }

    val (offset, length, cycle): Cycle =
      rotateForever.find { case (_, _, _, maybeInt) => maybeInt.isDefined }.flatMap(_._4).get
    val mod = ((n * 4) - offset) % length
    cycle(mod.toInt - 1)
  }

  assert(scoreField(rotateLeft(parseInput(testInput)).map(tiltLeft)) == 136)
  assert(nRotations(rotateLeft(parseInput(testInput)), BigInt("1000000000")) == 64)

  val part1 = scoreField(rotateLeft(parseInput(providedInput)))
  println(s"Part 1: ${part1}")

  val part2 = nRotations(rotateLeft(parseInput(providedInput)), BigInt("1000000000"))
  println(s"Part 2: ${part2}")

}
