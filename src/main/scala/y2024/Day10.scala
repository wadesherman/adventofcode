package y2024

import model.{CardinalDirection, Node, Position}
import util.ProvidedInput

object Day10 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 10
  override val testInput: Array[String] = Array(
    "89010123",
    "78121874",
    "87430965",
    "96549874",
    "45678903",
    "32019012",
    "01329801",
    "10456732"
  )

  type Grid = Map[Position, Node[Int]]

  def parseInput(input: Array[String]): Grid =
    input.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (c, x) =>
        val p = Position(x, y)
        p -> Node(p, c.asDigit)
      }
    }.toMap

  type AllPaths = Seq[List[List[Node[Int]]]]
  def paths(grid: Grid): AllPaths = {
    def isGood: (List[List[Node[Int]]]) => List[List[Node[Int]]] = { paths =>
      val n = paths.flatMap(p =>
        p match {
          case Nil => Nil
          case ::(head, _) => {
            val newPaths = grid.get(head.position) match {
              case Some(n) =>
                n.position.adjacent
                  .filter(_.direction match {
                    case _: CardinalDirection => true
                    case _                    => false
                  })
                  .flatMap(p => grid.get(p.position))
                  .filter(_.value == head.value + 1)
                  .map(n => n :: p)
              case None => Nil
            }
            newPaths
          }
        }
      )
      if (n.exists(_.headOption.exists(_.value == 9))) n else isGood(n)
    }

    val trailScore = grid
      .collect { case (_, v) if v.value == 0 => v }
      .toList
      .map(n => isGood(List(List(n))))

    trailScore
  }

  def trailheadScore(paths: AllPaths): Int = paths.map(_.map(_.head).distinct.length).sum
  def trailRating(paths: AllPaths): Int = paths.map(_.distinct.length).sum

  assert(trailheadScore(paths(parseInput(testInput))) == 36)
  val part1 = run { trailheadScore(paths(parseInput(providedInput))) }
  println(s"Part 1: $part1")

  assert(trailRating(paths(parseInput(testInput))) == 81)
  val part2 = run { trailRating(paths(parseInput(providedInput))) }
  println(s"Part 2: $part2")

}
