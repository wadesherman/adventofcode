package y2023

import util.ProvidedInput

import scala.annotation.tailrec
import scala.util.Try

object Day10 extends App with ProvidedInput {
  val year = 2023
  val day = 10

  val testInput = Array(
    ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  )
  val testInput2 = Array(
    "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  )

  val testInput3 = Array(
    "7-F7-",
    ".FJ|7",
    "SJLL7",
    "|F--J",
    "LJ.LJ"
  )

  val testInput4 = Array(
    "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  )

  val testInput5 = Array(
    ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
  )

  class Field(input: Array[String]) {

    type Tiles = Array[Array[Tile]]

    case class Tile(
        v: String,
        x: Int,
        y: Int
    ) {
      def realV: String = {
        if (v == "S") {
          val a = above.filter(t => Seq("|", "7", "F").contains(t.v))
          val b = below.filter(t => Seq("|", "L", "J").contains(t.v))
          val l = left.filter(t => Seq("-", "L", "F").contains(t.v))
          val r = right.filter(t => Seq("-", "J", "7").contains(t.v))
          (a, b, l, r) match {
            case (_, _, None, None) => "|"
            case (None, None, _, _) => "-"
            case (None, _, None, _) => "F"
            case (None, _, _, None) => "7"
            case (_, None, None, _) => "L"
            case (_, None, _, None) => "J"
          }
        } else {
          v
        }
      }

      def above: Option[Tile] = Try {
        tiles(y - 1)(x)
      }.toOption

      def below: Option[Tile] = Try {
        tiles(y + 1)(x)
      }.toOption

      def left: Option[Tile] = Try {
        tiles(y)(x - 1)
      }.toOption

      def right: Option[Tile] = Try {
        tiles(y)(x + 1)
      }.toOption

      def getConnected: Seq[Tile] = {
        realV match {
          case "|" => Seq(above, below).flatten
          case "-" => Seq(left, right).flatten
          case "7" => Seq(left, below).flatten
          case "F" => Seq(right, below).flatten
          case "J" => Seq(left, above).flatten
          case "L" => Seq(right, above).flatten
        }
      }
    }

    val tiles: Tiles = {
      input.zipWithIndex.map { case (row, yi) =>
        row.zipWithIndex.map { case (c, xi) =>
          Tile(c.toString, xi, yi)
        }.toArray
      }
    }

    val startPosition: Tile = tiles.flatten.find(_.v == "S").get

    val path: Set[(Int, Int)] = {
      val starts = startPosition.getConnected
      starts.head :: traverseFence(starts.head, startPosition)
    }.map(t => (t.x, t.y)).toSet

    @tailrec
    private def traverseFence(thisTile: Tile, lastTile: Tile, l: List[Tile] = Nil): List[Tile] = {
      if (thisTile.v == "S") {
        l
      } else {
        val next = thisTile.getConnected.filter(_ != lastTile)
        traverseFence(next.head, thisTile, l :+ next.head)
      }
    }

    def part1: Int = path.size / 2

    def part2: Int = {
      val xmax = tiles(0).length - 1
      val ymax = tiles.length - 1
      val yrange = for { y <- 0 to ymax } yield (xmax, y, Seq(y, xmax).min)
      val xrange = for { x <- 0 to xmax } yield (x, ymax, Seq(x, ymax).min)
      // lazily delete the duplicate diagonal from the bottom right
      val startingPoints = (yrange ++ xrange).toSet

      startingPoints.foldLeft(0) {
        case (z, (x, y, l)) => {
          val steps = (0 to l)
          val diagonalCount = steps
            .foldLeft((0, false)) {
              case ((count, isInside), i) => {
                val tile = tiles(y - i)(x - i)
                if (path.contains(tile.x, tile.y)) {
                  if (Seq("|", "-", "J", "F").contains(tile.realV)) {
                    (count, !isInside)
                  } else {
                    (count, isInside)
                  }
                } else {
                  if (isInside) {
                    (count + 1, isInside)
                  } else {
                    (count, isInside)
                  }
                }
              }
            }
            ._1
          z + diagonalCount
        }
      }

    }
  }

  val test1Field = new Field(testInput)
  val test2Field = new Field(testInput2)
  val test3Field = new Field(testInput3)
  val test4Field = new Field(testInput4)
  val test5Field = new Field(testInput5)
  val providedField = new Field(providedInput)

  assert(test1Field.part1 == 4)
  assert(test2Field.part1 == 8)
  assert(test3Field.part1 == 8)
  assert(test4Field.part2 == 4)
  assert(test5Field.part2 == 8)

  println(s"Part 1: ${providedField.part1}")
  println(s"Part 2: ${providedField.part2}")
}
