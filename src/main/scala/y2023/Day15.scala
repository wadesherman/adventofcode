package y2023

import util.ProvidedInput

object Day15 extends App with ProvidedInput {
  val year = 2023
  val day = 15
  val testInput = Array(
    "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  )

  def parseInput(input: Array[String]): Array[String] =
    input(0).split(",")

  def hash(str: String): Int = {
    str.foldLeft(0) {
      case (z, c) => {
        ((z + c.toInt) * 17) % 256
      }
    }
  }

  sealed trait Op {
    val label: String
    val box: Int
  }
  case class Add(label: String, box: Int, focalLength: Int) extends Op
  case class Remove(label: String, box: Int) extends Op

  val toOp: String => Op = {
    case s"$label-"             => Remove(label, hash(label))
    case s"$label=$focalLength" => Add(label, hash(label), focalLength.toInt)
  }

  type Boxes = Map[Int, Seq[Add]]
  def moveLenses(ops: Array[Op]): Boxes = {
    ops.foldLeft(Map.empty[Int, Seq[Add]]) {
      case (map, op) => {
        op match {
          case a: Add =>
            map.get(a.box) match {
              case None => map ++ Map(a.box -> Seq(a))
              case Some(lenses) =>
                lenses.find(_.label == a.label) match {
                  case None => map ++ Map(a.box -> (lenses :+ a))
                  case Some(existingLens) =>
                    map ++ Map(a.box -> lenses.map(l => if (l.label == existingLens.label) a else l))
                }
            }
          case r: Remove =>
            map.get(r.box) match {
              case Some(lenses) => map ++ Map(r.box -> lenses.filter(_.label != r.label))
              case None         => map
            }
        }
      }
    }
  }

  def score(boxes: Boxes): Int = boxes.flatMap {
    case (box, lenses) => {
      lenses.zipWithIndex.map {
        case (l, i) => {
          (box + 1) * (i + 1) * l.focalLength
        }
      }
    }
  }.sum

  val parsedTestInput = parseInput(testInput)
  assert(parsedTestInput.map(hash).sum == 1320)
  assert(score(moveLenses(parsedTestInput.map(toOp))) == 145)

  val parsedInput = parseInput(providedInput)
  val part1 = parsedInput.map(hash).sum
  val part2 = score(moveLenses(parsedInput.map(toOp)))
  println(s"Part 1: ${part1}")
  println(s"Part 2: ${part2}")
}
