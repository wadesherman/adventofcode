package y2023

import util.ProvidedInput

import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

object Day12 extends App with ProvidedInput {
  val year = 2023
  val day = 12
  val testInput = Array(
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  )

  type Pattern = Array[String]

  case class Count(
      curr: Option[Int],
      acc: Seq[Int],
      copies: BigInt
  ) {
    def ignore: Count = curr match {
      case Some(value) => Count(None, acc :+ value, copies)
      case None        => this
    }
    def increment: Count = curr match {
      case Some(_) => this.copy(curr = curr.map(_ + 1))
      case None    => this.copy(curr = Some(1))
    }

    def isValid(target: Count): Boolean = {
      acc == target.acc.take(acc.length) &&
      ((curr, target.acc.drop(acc.length).headOption) match {
        case (Some(v), Some(t)) => v <= t
        case _                  => true
      })
    }
  }

  object Count {
    def empty: Count = Count(None, Nil, 1)
  }

  def parseInput(input: Array[String]): Array[(Pattern, Count)] = input
    .map { case s"${pattern} ${c}" =>
      (
        pattern.map(_.toString).toArray,
        Count(None, c.split(",").map(_.toInt), 1)
      )
    }

  def unfold(input: (Pattern, Count), m: Int): (Pattern, Count) = {
    val (p, t) = input
    val newP: Pattern = Seq.fill(m)(p).reduce((l, r) => l ++ List("?") ++ r)
    val newT: Count = Count(curr = None, acc = Seq.fill(m)(t.acc).flatten, 1)
    (newP, newT)
  }

  val tp = Executors.newFixedThreadPool(10)
  implicit val ec =
    ExecutionContext.fromExecutor(tp)

  def part1(input: Array[(Pattern, Count)]): BigInt = {
    val futures: Seq[Future[BigInt]] = input.toSeq.map(r => Future { count(r) })
    val seq = Future.sequence(futures)
    Await.result(seq, 10.seconds)
  }.sum



  def part2(input: Array[(Pattern, Count)]): BigInt = {
    val futures: Seq[Future[BigInt]] = input.toSeq.map(unfold(_, 5)).map(r => Future { count(r) })
    val seq: Future[Seq[BigInt]] = Future.sequence(futures)
    Await.result(seq, 10.seconds)
  }.sum

  def count(r: (Pattern, Count)): BigInt = {
      val (p, t) = r

      p.foldLeft(Seq(Count.empty)) {
        case (acc, l) => {
          val chars: Seq[String] = {
            if (l == "?") {
              Seq(".", "#")
            } else {
              Seq(l)
            }
          }
          (for {
            s <- acc
            c <- chars
          } yield {
            if (c == ".") {
              s.ignore
            } else {
              s.increment
            }
          }).filter(_.isValid(t))
            .groupBy(g => g)
            .map(f => f._1.copy(copies = f._2.map(_.copies).sum))
            .toSeq
        }
      }.map(_.ignore)
        .filter(_.acc == t.acc)
    }.map(_.copies).sum


  val parsedTestInput = parseInput(testInput)

  assert(part1(parsedTestInput) == 21)
  assert(part2(parsedTestInput) == 525152)

  val parsedProvidedInput = parseInput(providedInput)
  println(s"Part 1: ${part1(parsedProvidedInput)}")

  val part2Result = run { part2(parsedProvidedInput) }
  println(s"Part 2: $part2Result")

  tp.shutdown()
}
