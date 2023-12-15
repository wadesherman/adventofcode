package y2023

import util.ProvidedInput

import scala.annotation.tailrec

object Day8 extends App with ProvidedInput {
  val year = 2023
  val day = 8
  val testInput = Array(
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val testInput2 = Array(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val testInput3 = Array(
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  )

  case class Instructions(
      v: String
  )
  case class Node(
      v: String,
      l: String,
      r: String
  )

  def parseInput(input: Array[String]): (Instructions, Map[String, Node]) = {
    val instructions = Instructions(input(0))
    val nodes: Map[String, Node] = input
      .slice(2, input.length)
      .map { case s"${node} = (${left}, ${right})" =>
        node -> Node(node, left, right)
      }
      .toMap
    (instructions, nodes)
  }

  def countUntil(
      parsedInput: (Instructions, Map[String, Node]),
      start: String,
      exitCondition: String => Boolean
  ): BigInt = {
    val (instructions, nodes) = parsedInput
    val startNode = nodes(start)
    val instructionsLength = instructions.v.length

    @tailrec
    def search(
        node: Node,
        exitCondition: String => Boolean,
        count: BigInt = 0,
        instructionsIndex: Int = 0
    ): BigInt = {
      val newCount = count + 1
      val newIndex = (instructionsIndex + 1) % instructionsLength
      val instruction = instructions.v(instructionsIndex)
      val nextNode = {
        if (instruction == 'R') {
          node.r
        } else {
          node.l
        }
      }
      if (exitCondition(nextNode)) {
        newCount
      } else {
        search(nodes(nextNode), exitCondition, newCount, newIndex)
      }
    }

    search(startNode, exitCondition)
  }

  def countUntilMany(
      parsedInput: (Instructions, Map[String, Node]),
      startingPoints: String => Boolean,
      exitCondition: String => Boolean
  ): Seq[BigInt] = {
    val (_, nodes) = parsedInput
    val startingNodes = nodes.keys.filter(startingPoints)
    startingNodes
      .map(n => countUntil(parsedInput, n, exitCondition))
      .toSeq
  }

  /** https://en.wikipedia.org/wiki/Least_common_multiple#Using_the_greatest_common_divisor
    */
  def lcm(list: Seq[BigInt]): BigInt =
    list.foldLeft(BigInt(1)) { (a, b) => b * a / b.gcd(a) }

  assert(countUntil(parseInput(testInput), "AAA", _ == "ZZZ") == 2)
  assert(countUntil(parseInput(testInput2), "AAA", _ == "ZZZ") == 6)
  assert(lcm(countUntilMany(parseInput(testInput3), _.endsWith("A"), _.endsWith("Z"))) == 6)

  val input = parseInput(providedInput)
  val part1 = countUntil(input, "AAA", _ == "ZZZ")
  println(s"Part 1: ${part1}")

  val part2 = lcm(countUntilMany(input, _.endsWith("A"), _.endsWith("Z")))
  println(s"Part 2: ${part2}")
}
