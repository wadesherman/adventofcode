package y2024

import model._
import util.ProvidedInput

import scala.annotation.tailrec

object Day12 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 12
  override val testInput: Array[String] = Array(
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE"
  )

  type Grid = Map[Position, Node[Char]]
  def parseInput(input: Array[String]): Grid = {
    input.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (c, x) =>
        Position(x, y) -> Node(Position(x, y), c)
      }
    }.toMap
  }

  case class Plot(node: Node[Char], fences: Int, fenceNeighbors: List[model.Vector])
  type Plots = List[Plot]
  type Region = List[Plots]

  def plots(grid: Grid): Region = {
    val groups: List[List[Plot]] = Nil
    val visited: Set[Position] = Set.empty[Position]

    @tailrec
    def collectGroup(nodes: List[Node[Char]], visited: Set[Plot]): Plots = {
      if (nodes.isEmpty) {
        visited.toList
      } else {
        val acc: List[(Plot, List[Node[Char]])] = nodes
          .filter(n => !visited.map(_.node).contains(n))
          .map(n => {
            val allNeighbors = n.position.cardinalNeighbors
            val nodeNeighbors = allNeighbors.flatMap(p => grid.get(p.position)).filter(_.value == n.value)
            val fenceNeighbors = allNeighbors.filter(an => !nodeNeighbors.map(_.position).contains(an.position))
            val fenceCount = fenceNeighbors.length
            (Plot(n, fenceCount, fenceNeighbors), nodeNeighbors)
          })
        collectGroup(acc.flatMap(_._2).distinct, visited ++ acc.map(_._1))
      }
    }

    val plots = grid.foldLeft((groups, visited)) {
      case ((g, v), (_, n)) => {
        if (v.contains(n.position)) {
          (g, v)
        } else {
          val newGroup: List[Plot] = collectGroup(List(n), Set.empty[Plot])
          (g :+ newGroup, v ++ newGroup.map(_.node.position))
        }
      }
    }
    plots._1
  }

  def countSides(v: List[model.Vector], d: Direction): Int = {
    val comp: (Vector, Vector) => Boolean = { case (last, n) =>
      d match {
        case N | S => last.position.x == n.position.x - 1 && last.position.y == n.position.y
        case E | W => last.position.x == n.position.x && last.position.y == n.position.y - 1
      }
    }
    val sorted = d match {
      case N | S => v.sortBy(n => (n.position.y, n.position.x))
      case E | W => v.sortBy(n => (n.position.x, n.position.y))
    }
    sorted match {
      case Nil => 0
      case head :: next => {
        next
          .foldLeft((head, 1)) {
            case ((last, c), n) => {
              if (comp(last, n)) {
                (n, c)
              } else {
                (n, c + 1)
              }
            }
          }
          ._2
      }
    }
  }

  def cost(region: Region): Int = {
    region
      .map(p => {
        val area = p.length
        val fences = p.map(_.fences).sum
        area * fences
      })
      .sum
  }

  def bulkCost(region: Region): Int = {
    region
      .map(p => {
        val area = p.length
        val fences = p
          .filter(_.fences > 0)
          .flatMap(_.fenceNeighbors)
          .groupBy(_.direction)
          .map { case (d, v) => countSides(v, d) }
          .sum
        area * fences
      })
      .sum
  }

  assert(cost(plots(parseInput(testInput))) == 1930)
  val part1 = run { cost(plots(parseInput(providedInput))) }
  println(s"Part 1: $part1")

  assert(bulkCost(plots(parseInput(testInput))) == 1206)
  val part2 = run { bulkCost(plots(parseInput(providedInput))) }
  println(s"Part 2: $part2")

}
