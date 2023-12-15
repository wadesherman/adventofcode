package y2023

import util.ProvidedInput

object Day11 extends App with ProvidedInput {
  val year = 2023
  val day = 11
  val testInput = Array(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )

  sealed trait Node {
    val x: Int
    val y: Int
  }

  case class Galaxy(
      x: Int,
      y: Int
  ) extends Node

  case class Space(
      x: Int,
      y: Int
  ) extends Node

  case class Expanded(
      x: Int,
      y: Int
  ) extends Node

  case class Edge(
      l: Galaxy,
      r: Galaxy,
      d: Int
  )

  def parseInput(input: Array[String]): Array[Array[Node]] = {
    val emptyCols: Seq[Int] = input(0).zipWithIndex
      .collect {
        case (_, xi) if input.indices.forall(yi => input(yi)(xi) == '.') => xi
      }

    input.zipWithIndex
      .map {
        case (r, y) => {
          {
            if (r.forall(_ == '.')) {
              r.map(_ => 'x')
            } else {
              r
            }
          }.zipWithIndex.map { case (c, x) =>
            {
              if (emptyCols.contains(x)) {
                'x'
              } else {
                c
              }
            } match {
              case '.' => Space(x, y)
              case '#' => Galaxy(x, y)
              case 'x' => Expanded(x, y)
            }
          }.toArray
        }
      }
  }

  type GEE = (Array[Galaxy], Seq[Edge], (Seq[Int], Seq[Int]))
  def galaxiesEdgesAndExpansions(input: Array[Array[Node]]): GEE = {
    val galaxies: Array[Galaxy] = input.flatten
      .collect { case g: Galaxy => g }

    val edges: Seq[Edge] = galaxies
      .combinations(2)
      .map(_.toList)
      .map { case l :: r :: Nil =>
        val d = (l.x - r.x).abs + (l.y - r.y).abs
        Edge(l, r, d)
      }
      .toSeq

    val expandedXIndices = input(0).collect { case e: Expanded => e }.map(_.x).toSeq

    val expandedYIndices = input.indices
      .map(yi => input(yi)(0))
      .collect { case e: Expanded => e }
      .map(_.y)

    (galaxies, edges, (expandedXIndices, expandedYIndices))
  }

  def measureAll(edges: Seq[Edge], expandedIndices: (Seq[Int], Seq[Int]), factor: Int): Seq[BigInt] = {
    val (x, y) = expandedIndices
    edges.map(e => {
      val countExpandedX: Int = e.r.x.compare(e.l.x) match {
        case 0      => 0
        case s: Int => Range(e.l.x, e.r.x, s).intersect(x).length
      }
      val countExpandedY = e.r.y.compare(e.l.y) match {
        case 0      => 0
        case s: Int => Range(e.l.y, e.r.y, s).intersect(y).length
      }
      val t = countExpandedX + countExpandedY
      BigInt(e.d) + (t * factor) - t
    })
  }

  def part1(edges: Seq[Edge], expandedIndices: (Seq[Int], Seq[Int])): BigInt =
    measureAll(edges, expandedIndices, 2).sum

  def part2(edges: Seq[Edge], expandedIndices: (Seq[Int], Seq[Int]), factor: Int): BigInt =
    measureAll(edges, expandedIndices, factor).sum

  val parsedTestInput = parseInput(testInput)
  val (_, testE, testI) = galaxiesEdgesAndExpansions(parsedTestInput)
  assert(part1(testE, testI) == 374)
  assert(part2(testE, testI, 10) == 1030)
  assert(part2(testE, testI, 100) == 8410)

  val parsedProvidedInput = parseInput(providedInput)
  val (_, pE, pI) = galaxiesEdgesAndExpansions(parsedProvidedInput)
  println(s"Part 1: ${part1(pE, pI)}")
  println(s"Part 2: ${part2(pE, pI, 1000000)}")

}
