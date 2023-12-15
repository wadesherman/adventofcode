package y2023

import util.ProvidedInput

object Day4 extends App with ProvidedInput {
  val year = 2023
  val day = 4
  val testInput = Array(
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  )

  case class Scratcher(
      index: Int,
      score: Int,
      matchCount: Int,
      winningNumbers: Seq[Int],
      myNumbers: Seq[Int],
      wonCopies: Seq[Scratcher]
  )

  def parseInput(data: Seq[String]): Array[Scratcher] = data
    .map(_.split(':').toList)
    .map { case card :: numbers :: Nil =>
      val i = card.replaceAll("Card\\s+", "").toInt
      val lists: List[Seq[Int]] = numbers
        .split('|')
        .map(s => s.stripLeading().stripTrailing())
        .toList
        .map(l => l.replaceAll("\\s+", " "))
        .map(_.split(" ").map(_.toInt).toSeq)

      lists match {
        case winning :: mine :: Nil =>
          val m = matches(winning, mine)
          Scratcher(
            index = i,
            winningNumbers = winning,
            myNumbers = mine,
            matchCount = m,
            score = score(m),
            wonCopies = Nil
          )
      }
    }
    .toArray

  def matches(winningNumbers: Seq[Int], myNumbers: Seq[Int]): Int = {
    myNumbers.intersect(winningNumbers).length
  }

  def score(wins: Int): Int = {
    if (wins == 0) {
      0
    } else {
      scala.math.pow(2, wins - 1).toInt
    }
  }

  def part1Score(scratchers: Seq[Scratcher]): Int = scratchers.map(_.score).sum

  class WinTree(data: Array[Scratcher]) {

    def makeCopies(s: Scratcher): Scratcher = {
      if (s.score == 0) {
        s
      } else {
        s.copy(wonCopies = data.slice(s.index, s.index + s.matchCount).map(makeCopies))
      }
    }

    def countAll(s: Scratcher): Int = {
      1 + s.wonCopies.map(countAll).sum
    }

    val part2Answer: Int = data.map(makeCopies).map(countAll).sum

  }

  assert(part1Score(parseInput(testInput)) == 13)
  val part1Answer = part1Score(parseInput(providedInput))
  println(s"Part 1: $part1Answer")

  val testTree = new WinTree(parseInput(testInput))
  assert(testTree.part2Answer == 30)
  val part2Tree = new WinTree(parseInput(providedInput))
  println(s"Part 2: ${part2Tree.part2Answer}")

}
