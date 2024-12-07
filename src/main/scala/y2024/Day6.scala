package y2024

import model.{Node, Position}
import util.ProvidedInput

import scala.annotation.tailrec

object Day6 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 6
  override val testInput: Array[String] = Array(
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
  )

  type Grid = Map[Position, Node[Char]]

  def parseInput(input: Array[String]): Grid = {
    input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (c, x) =>
        Position(x, y) -> Node(Position(x, y), c)
      }
    }.toMap
  }

  def travelPath(grid: Grid): List[model.Vector] = {

    def travelTo(v: model.Vector): List[model.Vector] =
      grid.get(v.position) match {
        case None => Nil
        case Some(_) => {
          val peek = grid.get(v.next.position)
          peek match {
            case Some(node) if node.value == '#' => v :: travelTo(v.right)
            case _ => v :: travelTo(v.next)
          }
        }
      }

    val start = grid.find(_._2.value == '^').get._2
    val path = travelTo(model.Vector(start.position, model.N))
    path
  }

  def countVisited(path: List[model.Vector]): Int = path.map(_.position).distinct.size

  def proposeObstacles(path: List[model.Vector], grid: Grid) = {

    @tailrec
    def findLoop(visited: Set[model.Vector], current: model.Vector, newGrid: Grid): Boolean = {
      if (visited.contains(current)) {
        true // in a loop
      } else {
        newGrid.get(current.position) match {
          case None => false // off the grid
          case Some(_) => {
            val peek = newGrid.get(current.next.position)
            peek match {
              case None => false // off the grid
              case Some(node) if node.value == '#' => findLoop(visited + current, current.right, newGrid) // turn
              case _ => findLoop(visited + current, current.next, newGrid) // keep going
            }
          }
        }
      }
    }

    path.foldLeft(Set.empty[model.Vector], Set.empty[Position]){ case ((visited, proposed), current) =>
      if (
        grid.contains(current.next.position) && // next spot is on grid
          !visited.exists(_.position == current.next.position) && // we havent visited it yet
          findLoop(visited + current, current.right, grid + (current.next.position -> Node(current.next.position, '#'))) // we place an obstacle and search for a loop
      ) {
        (visited + current, proposed + current.next.position)
      } else {
        (visited + current, proposed)
      }
    }
  }


  assert(countVisited(travelPath(parseInput(testInput))) == 41)
  val part1 = countVisited(travelPath(parseInput(providedInput)))
  println(s"Part 1: $part1")

  val part2TestGrid = parseInput(testInput)
  assert(proposeObstacles(travelPath(part2TestGrid), part2TestGrid)._2.size == 6)
  val part2ProvidedGrid = parseInput(providedInput)
  val part2 = proposeObstacles(travelPath(part2ProvidedGrid), part2ProvidedGrid)._2.size
  println(s"Part 2: $part2")
}
