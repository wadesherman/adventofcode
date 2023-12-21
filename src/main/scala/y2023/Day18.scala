package y2023

import util.ProvidedInput
import util.Utils.hexToBigInt

import scala.annotation.tailrec

object Day18 extends App with ProvidedInput {
  val year = 2023
  val day = 18
  val testInput = Array(
    "R 6 (#70c710)",
    "D 5 (#0dc571)",
    "L 2 (#5713f0)",
    "D 2 (#d2c081)",
    "R 2 (#59c680)",
    "D 2 (#411b91)",
    "L 5 (#8ceee2)",
    "U 2 (#caa173)",
    "L 1 (#1b58a2)",
    "U 2 (#caa171)",
    "R 2 (#7807d2)",
    "U 3 (#a77fa3)",
    "L 2 (#015232)",
    "U 2 (#7a21e3)"
  )

  case class Coord(x: BigInt, y: BigInt)
  case class Instruction(
      dir: Char,
      steps: BigInt,
      color: String
  )

  case class Border(
      coord: Coord,
      color: String
                   )

  def parseInput(input: Array[String]): List[Instruction] =
    input.map { case s"$dir $steps (#$color)" => Instruction(dir.head, steps.toInt, color) }.toList

  def swapParameters(instructions: List[Instruction]): List[Instruction] = {
    instructions.map(i =>
      Instruction(
        dir = digitToDir(i.color.last),
        steps = hexToBigInt(i.color.take(5)),
        color = ""
      )
    )
  }

  def digitToDir(d: Char): Char = d match {
    case '0' => 'R'
    case '1' => 'D'
    case '2' => 'L'
    case '3' => 'U'
  }

  @tailrec
  def dig(instructions: List[Instruction], path: List[Border]): List[Border] = {
    if(instructions.isEmpty){
      path
    } else {
      val next: Instruction = instructions.head
      val currentPos: Border = path.head
      val op: Coord => Coord = next.dir match {
        case 'U' => {c => c.copy(y = c.y - 1)}
        case 'D' => {c => c.copy(y = c.y + 1)}
        case 'R' => {c => c.copy(x = c.x + 1)}
        case 'L' => {c => c.copy(x = c.x - 1)}
      }
      val start = currentPos.copy(coord = op(currentPos.coord), color = next.color)
      val newPath = List.iterate(start, next.steps.toInt)(o => o.copy(coord = op(o.coord), color = next.color)).reverse
      dig(instructions.tail, newPath ++ path.tail)
    }
  }

  def nextCoords(coord: Coord, instruction: Instruction): Coord = {
    instruction.dir match {
      case 'R' => coord.copy(x = coord.x + instruction.steps)
      case 'L' => coord.copy(x = coord.x - instruction.steps)
      case 'U' => coord.copy(y = coord.y - instruction.steps)
      case 'D' => coord.copy(y =  coord.y + instruction.steps)
    }
  }

  def shoelaceAndPick(instructions: List[Instruction]): BigInt = {
    val grouped = instructions.sliding(2, 2).map { case l :: r :: Nil => (l, r) }.toSeq
    val interior = grouped.foldLeft((BigInt(0), Coord(BigInt(0), BigInt(0)), true)) {
      case ((area, coord, plusminus), (l, r)) => {
        val c0 = nextCoords(coord, l)
        val c1 = nextCoords(c0, r)

        (area + ((c0.x * c1.y) - (c1.x * c0.y)), c1, !plusminus)
      }
    }._1
    val border = instructions.map(_.steps).sum + 4
    (interior + (border / 2) - 1)
  }

  assert(shoelaceAndPick(parseInput(testInput)) == 62)
  assert(shoelaceAndPick(swapParameters(parseInput(testInput))) == BigInt("952408144115"))
  println(s" Part 1: ${shoelaceAndPick(parseInput(providedInput))}")
  println(s" Part 2: ${shoelaceAndPick(swapParameters(parseInput(providedInput)))}")

}
