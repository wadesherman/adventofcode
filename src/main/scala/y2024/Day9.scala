package y2024

import util.ProvidedInput

import scala.annotation.tailrec

object Day9 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 9
  override val testInput: Array[String] = Array(
    "2333133121414131402"
  )

  sealed trait Disk {
    val id: Long
    val size: Long
  }

  case class Space(size: Long) extends Disk {
    val id = 0
    def split(newSize: Long): (Space, Space) =
      (Space(newSize), Space(size - newSize))
  }
  case class File(size: Long, id: Long) extends Disk {
    def split(newSize: Long): (File, File) =
      (File(newSize, id), File(size - newSize, id))
  }

  def parseInput(input: Array[String]): List[Disk] = {
    input.head.toList
      .map(_.asDigit)
      .zipWithIndex
      .foldLeft((List.empty[Disk], 0)) {
        case ((acc, id), (l, i)) => {
          if (i % 2 == 0) (acc :+ File(l, id), id + 1) else (acc :+ Space(l), id)
        }
      }
      ._1
  }

  def split(input: List[Disk]): (List[Disk], List[File]) = {
    val fileSize = input.map {
      case Space(_)      => 0
      case File(size, _) => size
    }.sum

    val split = input.foldLeft((List.empty[Disk], List.empty[File], fileSize, 0L)) {
      case ((start, end, remainingFiles, availableSpaces), d) => {
        if (remainingFiles <= availableSpaces) {
          d match {
            case f: File  => (start, f :: end, remainingFiles, availableSpaces)
            case Space(_) => (start, end, remainingFiles, availableSpaces)
          }

        } else {
          d match {
            case f: File =>
              if (remainingFiles - f.size < availableSpaces) {
                val (stay, move) = f.split(remainingFiles - availableSpaces)
                (start :+ stay, move :: end, remainingFiles - f.size, availableSpaces)
              } else {
                (start :+ d, end, remainingFiles - f.size, availableSpaces)
              }
            case Space(size) => (start :+ d, end, remainingFiles, availableSpaces + size)
          }
        }
      }
    }
    split._1 -> split._2
  }

  @tailrec
  def compact(data: (List[Disk], List[File]), foo: List[File]): List[File] = {
    val (disk, files) = data
    if (files.isEmpty) {
      foo ++ disk.collect { case f: File => f }
    } else {
      val (o, d, f): (File, List[Disk], List[File]) = disk.head match {
        case f: File => (f, disk.tail, files)
        case s: Space =>
          if (s.size == files.head.size) {
            (files.head, disk.tail, files.tail)
          } else if (s.size > files.head.size) {
            (files.head, s.split(files.head.size)._2 :: disk.tail, files.tail)
          } else {
            (files.head.split(s.size)._1, disk.tail, (files.head.split(s.size)._2 :: files.tail))
          }
      }
      compact((d, f), foo :+ o)
    }
  }


  @tailrec
  def defrag(disk: List[Disk], foo: List[Disk]): List[Disk] = {
    if(disk.isEmpty){
      foo
    } else {
      val files = disk.reverse.collect { case f: File => f }
      val (d, f) = disk.head match {
        case f: File => (disk.tail, foo :+ f)
        case s: Space => {
          files.find(_.size <= s.size) match {
            case Some(file) => {
              if(file.size == s.size){
                (disk.tail.map(d => {if(d == file) Space(file.size) else d}), foo :+ file)
              } else {
                val (_, move) = s.split(file.size)
                (move :: disk.tail.map(d => {if(d == file) Space(file.size) else d}), foo :+ file)
              }
            }
            case None => (disk.tail, foo :+ s)
          }
        }
      }
      defrag(d, f)
    }
  }

  def checkSum(input: List[Disk]): Long = {
    input
      .foldLeft((0L, 0L)) {
        case ((acc, i), d) => {
          (Seq.range[Long](i, i + d.size).map(_ * d.id).sum + acc, i + d.size)
        }
      }
      ._1
  }

  assert(checkSum(compact(split(parseInput(testInput)), Nil)) == 1928)
  val part1 = run { checkSum(compact(split(parseInput(providedInput)), Nil)) }
  println(s"Part 1: $part1")

  assert (checkSum(defrag(parseInput(testInput), Nil)) == 2858)
  val part2 = run { checkSum(defrag(parseInput(providedInput), Nil)) }
  println(s"Part 2: $part2")
}
