package y2024

import util.ProvidedInput

object Day13 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 13
  override val testInput: Array[String] = Array(
    "Button A: X+94, Y+34",
    "Button B: X+22, Y+67",
    "Prize: X=8400, Y=5400",
    "",
    "Button A: X+26, Y+66",
    "Button B: X+67, Y+21",
    "Prize: X=12748, Y=12176",
    "",
    "Button A: X+17, Y+86",
    "Button B: X+84, Y+37",
    "Prize: X=7870, Y=6450",
    "",
    "Button A: X+69, Y+23",
    "Button B: X+27, Y+71",
    "Prize: X=18641, Y=10279"
  )

  case class Prize(x: Long, y: Long)
  case class Move(x: Long, y: Long)
  case class Game(a: Move, b: Move, prize: Prize)

  def parseInput(input: Array[String]): List[Game] = {
    input
      .filter(_.nonEmpty)
      .sliding(3, 3)
      .map {
        case Array(a, b, p) => {
          val aMove = a match {
            case s"Button A: X+$x, Y+$y" => Move(x.toInt, y.toInt)
          }
          val bMove = b match {
            case s"Button B: X+$x, Y+$y" => Move(x.toInt, y.toInt)
          }
          val pPrize = p match {
            case s"Prize: X=$x, Y=$y" => Prize(x.toInt, y.toInt)
          }
          Game(aMove, bMove, pPrize)
        }
      }
      .toList
  }

  def play(game: Game): Option[(Long, Long)] = {

    val denominator = (game.a.x * game.b.y) - (game.b.x * game.a.y)
    val aNumerator = (game.b.y * game.prize.x) - (game.b.x * game.prize.y)
    val bNumerator = (-1 * game.a.y * game.prize.x) + (game.a.x * game.prize.y)

    if(
      denominator != 0 &&
        aNumerator % denominator == 0 &&
        bNumerator % denominator == 0
    ){
      val a = aNumerator / denominator
      val b = bNumerator / denominator
      Some(a, b)
    } else {
      None

    }
  }

  def fewestTokens(games: List[Game]): Long = {
    games.flatMap(g => play(g).map(r => r._1 * 3 + r._2)).sum
  }

  def correctInput(games: List[Game]): List[Game] = {
    games.map(g => g.copy(prize = Prize(g.prize.x + 10000000000000L, g.prize.y + 10000000000000L)))
  }

  assert(fewestTokens(parseInput(testInput)) == 480)
  val part1 = run { fewestTokens(parseInput(providedInput)) }
  println(s"Part 1: $part1")

  val part2 = run { fewestTokens(correctInput(parseInput(providedInput))) }
  println(s"Part 2: $part2")
}
