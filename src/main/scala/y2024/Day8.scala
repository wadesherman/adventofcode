package y2024

import model.{Node, Position}
import util.ProvidedInput

object Day8 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 8
  override val testInput: Array[String] = Array(
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
  )

  sealed trait Location

  case object Empty extends Location
  case class Antenna(id: Char) extends Location
  case class AntiNode(id: Char) extends Location

  def parseInput(input: Array[String]): List[Node[Location]] = {
    input.zipWithIndex.flatMap { case (r, y) =>
      r.zipWithIndex
        .map {
          case ('.', x) => Node[Location](Position(x, y), Empty)
          case (a, x)   => Node[Location](Position(x, y), Antenna(a))
        }
    }.toList
  }

  def dist(l: Position, r: Position): (Long, Long, Long, Long, Long, Long) = {
    (l.x, l.y, r.x, r.y, l.x - r.x, l.y - r.y)
  }

  type Calc = (Node[Antenna], Node[Antenna], Long, Long) => List[Node[AntiNode]]

  def simpleAntiNodes: Calc = { case (l, r, _, _) =>
    val (lX, lY, rX, rY, dX, dY) = dist(l.position, r.position)
    List(Node(Position(lX + dX, lY + dY), AntiNode(l.value.id)), Node(Position(rX - dX, rY - dY), AntiNode(r.value.id)))
  }

  def harmonicAntiNodes: Calc = {
    case (l, r, maxX, maxY) => {
      val (lX, lY, rX, rY, dX, dY) = dist(l.position, r.position)

      type I = ((Long, Long), (Long, Long))
      def iter(s: I)(f: I => I) = {
        Iterator
          .iterate(s)(f)
          .takeWhile { case ((x, y), _) => x >= 0 && x <= maxX && y >= 0 && y <= maxY }
      }

      (
        iter(((lX, lY), (dX, dY))) { case ((x, y), (dx, dy)) => ((x - dx, y - dy), (dx, dy)) } ++
          iter(((rX, rY), (dX, dY))) { case ((x, y), (dx, dy)) => ((x + dx, y + dy), (dx, dy)) }
      ).map { case ((x, y), _) => Node(Position(x, y), AntiNode(l.value.id)) }.toList

    }
  }

  def allAntiNodes(nodes: List[Node[Location]], calc: Calc) = {
    val maxX = nodes.map(_.position).map { case Position(x, _) => x }.max
    val maxY = nodes.map(_.position).map { case Position(_, y) => y }.max

    nodes
      .collect { case Node(p, a: Antenna) => Node(p, a) }
      .groupBy(_.value)
      .flatMap { case (_, locations) => locations.combinations(2) }
      .flatMap { case List(l, r) => calc(l, r, maxX, maxY) }
      .filter(n => n.position match { case Position(x, y) => x >= 0 && x <= maxX && y >= 0 && y <= maxY })
      .map(_.position)
      .toList
      .distinct
      .length
  }

  val parsedTestInput = parseInput(testInput)
  val parsedProvidedInput = parseInput(providedInput)

  assert(allAntiNodes(parsedTestInput, simpleAntiNodes) == 14)
  val part1 = run { allAntiNodes(parsedProvidedInput, simpleAntiNodes) }
  println(s"Part 1: $part1")

  assert(allAntiNodes(parsedTestInput, harmonicAntiNodes) == 34)
  val part2 = run { allAntiNodes(parsedProvidedInput, harmonicAntiNodes) }
  println(s"Part 2: $part2")

}
