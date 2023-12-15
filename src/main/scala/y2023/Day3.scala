package y2023

import util.ProvidedInput

object Day3 extends App with ProvidedInput {
  val year = 2023
  val day = 3
  val testInput = Array(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )

  type SchematicData = Array[String]

  case class Acc(sum: Int, isAdjacent: Boolean, num: String)

  class EngineSchematic(d: SchematicData) {

    case class Cell(v: Char, coord: Coord, isAdjacent: Boolean)

    case class Coord(x: Int, y: Int) {
      def tl: Coord = Coord(x - 1, y - 1)
      def tm: Coord = Coord(x, y - 1)
      def tr: Coord = Coord(x + 1, y - 1)
      def ml: Coord = Coord(x - 1, y)
      def mr: Coord = Coord(x + 1, y)
      def bl: Coord = Coord(x - 1, y + 1)
      def bm: Coord = Coord(x, y + 1)
      def br: Coord = Coord(x + 1, y + 1)
      def above: Seq[Cell] = Seq(tl, tm, tr).flatMap(maybeCell)
      def left: Option[Cell] = maybeCell(ml)
      def right: Option[Cell] = maybeCell(mr)
      def below: Seq[Cell] = Seq(bl, bm, br).flatMap(maybeCell)
      def allAdjacent: Seq[Cell] = above ++ left ++ right ++ below
    }

    def isSymbol(c: Char): Boolean = {
      !c.isDigit && c.!=('.')
    }

    def maybeCell(coord: Coord): Option[Cell] = {
      if (
        coord.x >= 0 &&
        coord.y >= 0 &&
        coord.y < d.length &&
        coord.x < d(coord.y).length
      ) {
        Some(Cell(v = d(coord.y)(coord.x), coord, false))
      } else {
        None
      }
    }

    def isAdjacent(coord: Coord): Boolean = {
      val adjacentCoords: Seq[Cell] = coord.allAdjacent
      adjacentCoords.exists(c => isSymbol(c.v))
    }

    val indexedData: Array[Cell] = d
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (row, y) =>
        row
          .map { case (v, x) => (v, Coord(x, y)) }
          .map { case (v, coord) => Cell(v, coord, isAdjacent(coord)) }
      }

    val sumPart1 =
      indexedData
        .foldLeft(Acc(0, false, "")) { case (acc, cell) =>
          if (!cell.v.isDigit) {
            if (acc.isAdjacent) {
              Acc(
                sum = acc.sum + acc.num.toInt,
                isAdjacent = false,
                num = ""
              )
            } else {
              acc.copy(isAdjacent = false, num = "")
            }

          } else {
            Acc(
              sum = acc.sum,
              isAdjacent = acc.isAdjacent || cell.isAdjacent,
              num = acc.num + cell.v
            )
          }
        }
        .sum

    def expandRight(cell: Cell, acc: String = ""): String = {
      cell.coord.right match {
        case Some(cellRight) if cellRight.v.isDigit =>
          expandRight(cellRight, acc + cellRight.v)
        case _ => acc
      }
    }

    def expandLeft(cell: Cell, acc: String = ""): String = {
      cell.coord.left match {
        case Some(cellLeft) if cellLeft.v.isDigit =>
          expandLeft(cellLeft, cellLeft.v + acc)
        case _ => acc
      }
    }

    def checkAboveBelow(cells: Seq[Cell]): Seq[Int] =
      if (cells.map(_.v.isDigit) == Seq(true, false, true)) {
        Seq(expandLeft(cells(1)).toInt, expandRight(cells(1)).toInt)
      } else {
        cells
          .find(_.v.isDigit)
          .map(digitCell => expandLeft(digitCell, expandRight(digitCell, digitCell.v.toString)))
          .map(_.toInt)
          .toSeq
      }

    def checkLeftRight(cell: Cell, side: Cell => Option[Cell], expander: Cell => String): Seq[Int] =
      if (side(cell).exists(_.v.isDigit)) {
        Seq(expander(cell).toInt)
      } else {
        Nil
      }

    def maybeGearRatio(c: Cell): Option[Int] = {
      val t: Seq[Int] = checkAboveBelow(c.coord.above)
      val b: Seq[Int] = checkAboveBelow(c.coord.below)

      val l = checkLeftRight(c, _.coord.left, expandLeft(_, ""))
      val r: Seq[Int] = checkLeftRight(c, _.coord.right, expandRight(_, ""))

      val all: Seq[Int] = (l ++ r ++ t ++ b).toList
      all match {
        case first :: second :: Nil => Some(first * second)
        case _                      => None
      }
    }

    val sumPart2: Int =
      indexedData.foldLeft(0) { case (runningSum, cell) =>
        if (cell.v == '*') {
          maybeGearRatio(cell) match {
            case Some(ratio) => runningSum + ratio
            case None        => runningSum
          }
        } else {
          runningSum
        }
      }
  }

  val testSchematic = new EngineSchematic(testInput)
  val realSchematic = new EngineSchematic(providedInput)

  assert(testSchematic.sumPart1 == 4361)
  val part1Answer = realSchematic.sumPart1
  println(s"Part 1: $part1Answer")

  assert(testSchematic.sumPart2 == 467835)
  val part2Answer = realSchematic.sumPart2
  println(s"Part 2: $part2Answer")

}
