package y2024

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

  sealed trait Direction
  case object N extends Direction
  case object S extends Direction
  case object E extends Direction
  case object W extends Direction
  case object NE extends Direction
  case object NW extends Direction
  case object SE extends Direction
  case object SW extends Direction

  case class Vector(pos: Pos, direction: Direction) {
    def next: Vector = direction match {
      case N  => this.copy(pos = pos.n)
      case S  => this.copy(pos.s)
      case E  => this.copy(pos.e)
      case W  => this.copy(pos.w)
      case NE => this.copy(pos.ne)
      case NW => this.copy(pos.nw)
      case SE => this.copy(pos.se)
      case SW => this.copy(pos.sw)
    }
  }

  case class Pos(x: Int, y: Int) {
    def n = Pos(x, y - 1)
    def s = Pos(x, y + 1)
    def e = Pos(x + 1, y)
    def w = Pos(x - 1, y)
    def ne = Pos(x + 1, y - 1)
    def nw = Pos(x - 1, y - 1)
    def se = Pos(x + 1, y + 1)
    def sw = Pos(x - 1, y + 1)

    def adjacent: List[Vector] = List(
      Vector(n, N),
      Vector(s, S),
      Vector(e, E),
      Vector(w, W),
      Vector(ne, NE),
      Vector(nw, NW),
      Vector(se, SE),
      Vector(sw, SW)
    )
  }
  case class Node(pos: Pos, value: Char)

  type Grid = Map[Pos, Node]

  def parseInput(input: Array[String]): Grid = {
    input.zipWithIndex.flatMap {
      case (row, y) => {
        row.zipWithIndex.map {
          case (value, x) => {
            Pos(x, y) -> Node(Pos(x, y), value)
          }
        }
      }
    }.toMap
  }

  def searchXMAS(grid: Grid) = {

    @tailrec
    def xmas(p: List[Node], v: Vector): Int = {
      if (p.map(_.value).mkString == "XMAS") {
        1
      } else {
        val n: Vector = v.next
        grid.get(v.pos) match {
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
          val adjacent = n.pos.adjacent
          adjacent.map(v => xmas(List(n), v)).sum
        } else {
          0
        }
      }
    }.sum
  }

  def searchX_MAS(grid: Grid): Int = {
    grid.map { case (_, node) =>
      if (
        node.value == 'A' &&
        ((grid.get(node.pos.nw).exists(_.value == 'M') && grid.get(node.pos.se).exists(_.value == 'S')) ||
          (grid.get(node.pos.nw).exists(_.value == 'S') && grid.get(node.pos.se).exists(_.value == 'M'))) &&
        ((grid.get(node.pos.ne).exists(_.value == 'M') && grid.get(node.pos.sw).exists(_.value == 'S')) ||
          (grid.get(node.pos.ne).exists(_.value == 'S') && grid.get(node.pos.sw).exists(_.value == 'M')))
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
