package y2023

import util.ProvidedInput

import scala.annotation.tailrec

object Day17 extends App with ProvidedInput {
  val year = 2023
  val day = 17
  val testInput = Array(
    "2413432311323",
    "3215453535623",
    "3255245654254",
    "3446585845452",
    "4546657867536",
    "1438598798454",
    "4457876987766",
    "3637877979653",
    "4654967986887",
    "4564679986453",
    "1224686865563",
    "2546548887735",
    "4322674655533"
  )

  sealed trait Direction {
    val coord: Coord
    def s: Direction
    def r: Direction
    def l: Direction
    val v: Int
  }

  case class U(coord: Coord) extends Direction {
    val v = 3
    override def r: Direction = R(coord.copy(x = coord.x + 1))
    override def l: Direction = L(coord.copy(x = coord.x - 1))
    override def s: Direction = U(coord.copy(y = coord.y - 1))
  }
  case class R(coord: Coord) extends Direction {
    val v = 1
    override def r: Direction = D(coord.copy(y = coord.y + 1))
    override def l: Direction = U(coord.copy(y = coord.y - 1))
    override def s: Direction = R(coord.copy(x = coord.x + 1))
  }
  case class D(coord: Coord) extends Direction {
    val v = 0
    override def r: Direction = L(coord.copy(x = coord.x - 1))
    override def l: Direction = R(coord.copy(x = coord.x + 1))
    override def s: Direction = D(coord.copy(y = coord.y + 1))
  }
  case class L(coord: Coord) extends Direction {
    val v = 2
    override def r: Direction = D(coord.copy(y = coord.y + 1))
    override def l: Direction = U(coord.copy(y = coord.y - 1))
    override def s: Direction = L(coord.copy(x = coord.x - 1))
  }

  case class Coord(x: Int, y: Int)
  sealed trait Heading{
    val d: Direction
    val c: Int
    def adjacent: List[Heading]
  }
  case class Crucible(d: Direction, c: Int) extends Heading {
    def adjacent: List[Crucible] = {
      if (c == 3) {
        List(d.l, d.r).map(d => Crucible(d, 1))
      } else {
        List(d.l, d.r).map(d => Crucible(d, 1)) :+ Crucible(d.s, c+1)
      }
    }
  }

  case class UltraCrucible(d: Direction, c: Int) extends Heading {
    def adjacent: List[UltraCrucible] = {
      if (c == 10) {
        List(d.l, d.r).map(d => UltraCrucible(d, 1))
      } else if(c < 4){
        List(UltraCrucible(d.s, c + 1))
      } else {
        List(d.l, d.r).map(d => UltraCrucible(d, 1)) :+ UltraCrucible(d.s, c + 1)
      }
    }
  }



  case class Lava(v: Int, coord: Coord)

  def parseInput(input: Array[String]): Map[Coord, Option[Lava]] = {
    input.zipWithIndex
      .flatMap { case (r, y) =>
        r.zipWithIndex.map { case (c, x) => Coord(x, y) -> Some(Lava(c.toString.toInt, Coord(x, y))) }
      }
      .toMap
      .withDefaultValue(None)
  }

  @tailrec
  def bfs(
           paths: List[(Heading, Option[Int], List[Lava])],
           end: Coord,
           map: Map[Coord, Option[Lava]],
           cache: Map[Heading, Int]
  ): List[List[Lava]] = {
    if (paths.isEmpty) {
      Nil
    } else if (paths.forall { case (_, sum, _) => sum.isDefined }) {
      paths.map { case (_, sum, visited) => visited }
    } else {
      val currentShortestPath = paths.filter(_._2.isDefined).minByOption(_._2)
      val terminalPaths = currentShortestPath.toList

      val currentMin: Option[Int] = currentShortestPath.flatMap(_._2)

      val newPaths = paths.filter(_._2.isEmpty)
        .flatMap {
          case (heading, _, visited) => {
            val next: List[(Heading, Lava)] = heading.adjacent // all adjacent nodes
              .collect(h => h -> map(h.d.coord)) // hydrate with lava data
              .collect { case (h, Some(l)) => h -> l } // filter out steps off the lava
              .filter{ case (h, _) => !visited.map(_.coord).contains(h.d.coord)} // filter out visited
              .filter{ case (h, l) => cache.get(h) match {  // stop if we've been here more efficiently
                case Some(cachedScore) => (l.v + visited.map(_.v).sum) <= cachedScore
                case None => true
              }}

            val checkEnds: List[(Heading, Option[Int], List[Lava])] = next.map{ case (heading, lava) => {
              val nv: List[Lava] = lava +: visited
              val ns = nv.map(_.v).sum
              if(lava.coord == end){
                (heading, Some(ns), nv)
              } else {
                (heading, None, nv)
              }
            }}

            currentMin match {
              case None => checkEnds
              case Some(min) => checkEnds.filter{ case (h, mi, v) => (v.map(_.v).sum) <= min }
            }
          }
        }
      val newCache = cache ++ newPaths.map(np => np._1 -> np._3.map(_.v).sum)
      val dedupedNewPaths = newPaths.groupBy(_._1).map{ case (_, paths) => paths.minBy(_._3.map(_.v).sum)}.toList
      bfs(terminalPaths ++ dedupedNewPaths, end, map, newCache)
    }
  }

  def findOptimalPath(start: Heading, map: Map[Coord, Option[Lava]]): Int = {
    val maxX = map.keys.map(_.x).max
    val maxY = map.keys.map(_.y).max
    val cache = Map.empty[Heading, Int]
    val s = (start, None, List(Lava(0, Coord(0, 0))))
    bfs(List(s), Coord(maxX, maxY), map, cache).map(_.map(_.v).sum).min
  }

  def part1(map: Map[Coord, Option[Lava]]): Int = {
    findOptimalPath(Crucible(R(Coord(0, 0)), c = 0), map)
  }

  def part2(map: Map[Coord, Option[Lava]]): Int = {
    findOptimalPath(UltraCrucible(R(Coord(0, 0)), c = 0), map)
  }

  assert(part1(parseInput(testInput)) == 102)
  assert(part2(parseInput(testInput)) == 94)

  val p1 = run { part1(parseInput(providedInput)) }
  println(s"Part 1: ${p1}")

  val p2 = run { part2(parseInput(providedInput)) }
  println(s"Part 2: ${p2}")

}
