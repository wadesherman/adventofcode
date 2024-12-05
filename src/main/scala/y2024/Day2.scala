package y2024

import util.ProvidedInput

object Day2 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 2
  override val testInput: Array[String] = Array(
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9",
  )

  def splitInput(input: Array[String]): Array[Array[Int]] = input
    .map(_.split("\\s+")
      .map(_.toInt)
    )

  sealed trait Rise{
    val v: Int
    def isSafe(next: Int): Boolean
    def next(next: Int): Rise = this match {
      case Increasing(_) => Increasing(next)
      case Decreasing(_) => Decreasing(next)
      case Flat(v) => Rise(v, next)
    }

  }

  case class Flat(v: Int) extends Rise {
    override def isSafe(next: Int): Boolean = false
  }
  case class Increasing(v: Int) extends Rise {
    override def isSafe(next: Int): Boolean = next > v && next <= (v + 3)
  }
  case class Decreasing(v: Int) extends Rise {
    override def isSafe(next: Int): Boolean = next < v && next >= (v - 3)
  }

  object Rise {
    def apply(l: Int, r: Int): Rise = {
      if (l == r) {
        Flat(l)
      } else if(l > r){
        Decreasing(l)
      } else {
        Increasing(l)
      }
    }
  }

  def isSafeLevel(level: Array[Int]): Boolean = {

    def eval(maybeLast: Option[Int], maybeDir: Option[Rise], nextLevel: Int): (Boolean, Some[Int], Option[Rise]) = {
      (maybeLast, maybeDir) match {
        case (None, None) => (true, Some(nextLevel), None) // first element
        case (Some(l), None) => { // second element
          val z: Rise = Rise(l, nextLevel)
          (z.isSafe(nextLevel), Some(nextLevel), Some(z.next(nextLevel)))
        }
        case (Some(_), Some(rise)) => (rise.isSafe(nextLevel), Some(nextLevel), Some(rise.next(nextLevel)))
      }
    }

    level.foldLeft((true, Option.empty[Int], Option.empty[Rise])){ case ((safe, maybeLast, maybeDir), value) =>
      if(!safe){
        (safe, None, None)
      } else {
        eval(maybeLast, maybeDir, value)
      }
    }._1
  }

  case class Level(r: Option[Rise], marginUsed: Boolean)

  def isSafeLevelWithMargin(level: Array[Int]) = {
    val ll = level.toList
    val (first, rest) = ll match {
      case head :: tail => head -> tail
    }

    val starts = List(
      Level(Some(Increasing(first)), false),
      Level(Some(Decreasing(first)), false),
      Level(None, true),
    )

    rest.foldLeft(starts){ case (acc, value) => {
      acc.flatMap(l => {
        if (l.r.isEmpty){
          List(
            Level(Some(Increasing(value)), true),
            Level(Some(Decreasing(value)), true)
          )
        } else {
          val a = l.marginUsed match {
            case false => Some(l.copy(marginUsed = true))
            case _ => None
          }
          val b = l.r.exists(_.isSafe(value)) match {
            case true => Some(l.copy(r = l.r.map(_.next(value))))
            case _ => None
          }
          List(a, b).flatten
        }
      })
    }}.nonEmpty
  }

  def countSafeLevels(levels: Array[Array[Int]], function: Array[Int] => Boolean) = levels.map(function).count(identity)


  assert(countSafeLevels(splitInput(testInput), isSafeLevel) == 2)
  val part1 = countSafeLevels(splitInput(providedInput), isSafeLevel)
  println(s"Part 1: $part1")

  assert(countSafeLevels(splitInput(testInput), isSafeLevelWithMargin) == 4)
  val part2 = countSafeLevels(splitInput(providedInput), isSafeLevelWithMargin)
  println(s"Part 2: $part2")

}
