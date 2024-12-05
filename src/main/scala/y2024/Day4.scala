package y2024

import model.{Node, Position, Vector}
import util.ProvidedInput

import scala.annotation.tailrec

object Day4 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 4
  override val testInput: Array[String] = Array(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX"
  )

  type Grid = Map[Position, Node[Char]]

  def parseInput(input: Array[String]): Grid = {
    input.zipWithIndex.flatMap {
      case (row, y) => {
        row.zipWithIndex.map {
          case (value, x) => {
            Position(x, y) -> Node(Position(x, y), value)
          }
        }
      }
    }.toMap
  }

  def searchXMAS(grid: Grid) = {

    @tailrec
    def xmas(p: List[Node[Char]], v: Vector): Int = {
      if (p.map(_.value).mkString == "XMAS") {
        1
      } else {
        val n: Vector = v.next
        grid.get(v.position) match {
          case Some(value) if ("XMAS".startsWith((p :+ value).map(_.value).mkString)) => {
            xmas(p :+ value, n)
          }
          case _ => 0
        }
      }
    }

    grid.map {
      case (_, n) => {
        if (n.value == 'X') {
          val adjacent = n.position.adjacent
          adjacent.map(v => xmas(List(n), v)).sum
        } else {
          0
        }
      }
    }.sum
  }

  def searchX_MAS(grid: Grid): Int = {

    def check(position: Position, value: Char): Boolean = grid.get(position).exists(_.value == value)

    grid.map { case (_, node) =>
      if (
        node.value == 'A' &&
        ((check(node.position.nw, 'M') && check(node.position.se, 'S')) ||
          (check(node.position.nw, 'S') && check(node.position.se, 'M'))) &&
        ((check(node.position.ne, 'M') && check(node.position.sw, 'S')) ||
          (check(node.position.ne, 'S') && check(node.position.sw, 'M')))
      ) {
        1
      } else {
        0
      }
    }.sum
  }

  assert(searchXMAS(parseInput(testInput)) == 18)
  val part1 = searchXMAS(parseInput(providedInput))
  println(s"Part 1: $part1")

  assert(searchX_MAS(parseInput(testInput)) == 9)
  val part2 = searchX_MAS(parseInput(providedInput))
  println(s"Part 2: $part2")

}
