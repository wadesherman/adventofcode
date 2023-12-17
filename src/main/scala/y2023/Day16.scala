package y2023

import util.ProvidedInput

import java.util.concurrent.Executors
import scala.annotation.tailrec
import scala.collection.immutable.Seq
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Day16 extends App with ProvidedInput {
  val year = 2023
  val day = 16
  val testInput = Array(
    """.|...\....""",
    """|.-.\.....""",
    """.....|-...""",
    """........|.""",
    """..........""",
    """.........\""",
    """..../.\\..""",
    """.-.-/..|..""",
    """.|....-|.\""",
    """..//.|...."""
  )

  val tp = Executors.newFixedThreadPool(10)
  implicit val ec =
    ExecutionContext.fromExecutor(tp)

  case class Coord(x: Int, y: Int)
  case class Move(
      coord: Coord,
      dir: Char
  ) {
    val op: Map[(Char, Char), Seq[Char]] = Map(
      ('L', '.') -> Seq('L'),
      ('R', '.') -> Seq('R'),
      ('U', '.') -> Seq('U'),
      ('D', '.') -> Seq('D'),
      ('L', '/') -> Seq('D'),
      ('R', '/') -> Seq('U'),
      ('U', '/') -> Seq('R'),
      ('D', '/') -> Seq('L'),
      ('L', '\\') -> Seq('U'),
      ('R', '\\') -> Seq('D'),
      ('U', '\\') -> Seq('L'),
      ('D', '\\') -> Seq('R'),
      ('L', '|') -> Seq('U', 'D'),
      ('R', '|') -> Seq('U', 'D'),
      ('U', '|') -> Seq('U'),
      ('D', '|') -> Seq('D'),
      ('L', '-') -> Seq('L'),
      ('R', '-') -> Seq('R'),
      ('U', '-') -> Seq('L', 'R'),
      ('D', '-') -> Seq('L', 'R')
    )

    def next(d: Char, v: Char): Seq[Move] = {
      op((d, v)).map {
        case 'L' => Move(Coord(coord.x - 1, coord.y), 'L')
        case 'R' => Move(Coord(coord.x + 1, coord.y), 'R')
        case 'U' => Move(Coord(coord.x, coord.y - 1), 'U')
        case 'D' => Move(Coord(coord.x, coord.y + 1), 'D')
      }
    }

    def reverse: Move = {
      dir match {
        case 'L' => this.copy(dir = 'R')
        case 'R' => this.copy(dir = 'L')
        case 'U' => this.copy(dir = 'D')
        case 'D' => this.copy(dir = 'U')
      }
    }

  }

  type CellMap = Map[Coord, Option[Char]]
  def parseInput(input: Array[String]): CellMap =
    input.zipWithIndex
      .flatMap { case (r, y) => r.zipWithIndex.map { case (c, x) => Coord(x, y) -> Some(c) } }
      .toMap
      .withDefaultValue(None)

  @tailrec
  def traverse(paths: Seq[Move], vmap: CellMap, hist: Set[Move]): Set[Move] = {
    if (paths.isEmpty) {
      hist
    } else {
      val nextCoords: Seq[Move] = paths.flatMap(h => {
        val currentValue = vmap(h.coord)
        currentValue match {
          case Some(v) => h.next(h.dir, v).filter(m => vmap(m.coord).isDefined).filter(m => !hist.contains(m))
          case None    => Nil
        }
      })
      traverse(nextCoords, vmap, hist ++ paths.toSet)
    }
  }

  def part1(map: CellMap): Int = {
    val path = traverse(Seq(Move(Coord(0, 0), 'R')), map, Set.empty[Move])
    path.map(_.coord).size
  }

  def mapToRange: (CellMap, Coord => Boolean, Char) => Seq[Move] = { (map, f, char) =>
    map.view.filterKeys(f).toSeq.map { case (k, _) => Move(Coord(k.x, k.y), char) }
  }

  def startingPoints(map: CellMap) = {
    val xmax = map.view.filterKeys(_.y == 0).toSeq.map(_._1.x).max
    val ymax = map.view.filterKeys(_.x == 0).toSeq.map(_._1.y).max

    mapToRange(map, _.x == 0, 'R') ++
      mapToRange(map, _.x == xmax, 'L') ++
      mapToRange(map, _.y == 0, 'D') ++
      mapToRange(map, _.y == ymax, 'U')
  }

  def part2BruteForce(map: CellMap): Int = {
    startingPoints(map).map(s => {
      traverse(Seq(s), map, Set.empty[Move]).map(_.coord).size
    })
  }.max

  def part2Parallel(map: CellMap): Int = {
    val futures: Seq[Future[Int]] = startingPoints(map).map(s =>
      Future {
        traverse(Seq(s), map, Set.empty[Move]).map(_.coord).size
      }
    )
    val seq = Future.sequence(futures)
    Await.result(seq, 10.seconds)
  }.max

  assert(part1(parseInput(testInput)) == 46)
  assert(part2BruteForce(parseInput(testInput)) == 51)

  println(s"Part 1: ${part1(parseInput(providedInput))}")
  val p2bf = run { part2BruteForce(parseInput(providedInput)) }
  println(s"Part 2 Sequential: $p2bf")
  val p2par = run { part2Parallel(parseInput(providedInput)) }
  println(s"Part 2 Parallel: $p2par")

  tp.shutdown()
}
