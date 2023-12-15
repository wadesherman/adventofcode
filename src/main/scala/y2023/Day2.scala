package y2023

import util.ProvidedInput

object Day2 extends App with ProvidedInput {
  val year = 2023
  val day = 2
  val testInput = Array(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )

  val gameRegex = "Game (\\d+): ".r

  type Game = (Int, Array[Array[(String, Int)]])

  def parseInput(input: Seq[String]): Seq[Game] = input
    .map(str => {
      val index = gameRegex.findFirstMatchIn(str).get.subgroups.head.toInt
      val data = str.replaceAll(gameRegex.toString(), "").split("; ")
      (index, data)
    })
    .map { case (index, data) =>
      val cubes = data
        .map(_.split(", "))
        .map(cubes => cubes.map(_.split(" ")).map(array => array(1) -> array(0).toInt))
      (index, cubes)

    }

  val part1Constraint: Map[String, Int] = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  ).withDefaultValue(0)

  def isLegal(sets: Array[Array[(String, Int)]]) =
    sets.forall(sets => sets.forall { case (color, count) => count <= part1Constraint(color) })

  def part1Total(games: Seq[Game]): Int = games.foldLeft(0) { case (z, (index, cubeSets)) =>
    if (isLegal(cubeSets)) {
      z + index
    } else {
      z
    }
  }

  def part2Total(games: Seq[Game]): Int = games.foldLeft(0) { case (z, (_, cubeSets)) =>
    z + cubeSets.flatten
      .groupBy { case (color, _) => color }
      .map { case (_, sets) => sets.map(_._2).max }
      .product
  }

  assert(part1Total(parseInput(testInput)) == 8)
  val part1Answer = part1Total(parseInput(providedInput))
  println(s"Part 1: $part1Answer")

  assert(part2Total(parseInput(testInput)) == 2286)
  val part2Answer = part2Total(parseInput(providedInput))
  println(s"Part 2: $part2Answer")

}
