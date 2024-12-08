package y2024

import util.ProvidedInput

import scala.annotation.tailrec

object Day5 extends ProvidedInput with App {

  override val year: Int = 2024
  override val day: Int = 5
  override val testInput: Array[String] = Array(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47",
  )

  case class Rule(page: Int, before: Int)
  type RuleBook = Map[Int, Set[Int]]
  type Updates = List[List[Int]]
  type Pages = List[Int]


  def parseInput(input: Array[String]): (RuleBook, Updates) = {
    val (rules, pages) = input
      .toList
      .partition(_.contains('|'))

    (rules.map(_.split('|'))
      .map(r => Rule(r(0).toInt, r(1).toInt))
      .groupBy(_.page)
      .map{ case(k, v) => k -> v.map(_.before).toSet }
      ,
      pages.filter(_.nonEmpty).map(_.split(',').map(_.toInt).toList)
    )
  }

  def validateAll(input: (RuleBook, Updates)): Seq[(Pages, Boolean)] = {
    val (rules, updates) = input
    (updates.map(pages => validateOne(rules, pages)))
  }

  def validateOne(rules: RuleBook, pages: Pages) = {
    pages.foldLeft(List.empty[Int] -> true) { case ((visited, valid), page) => {
      val rr: Set[Int] = rules.getOrElse(page, Set.empty[Int])
      if (!valid || visited.exists(rr.contains)) {
        (visited :+ page, false)
      } else {
        (visited :+ page, true)
      }
    }
    }
  }

  def sumMiddlePagesofValidUpdates(input: (RuleBook, Updates)): Int = {
    validateAll(input)
      .collect{ case(pages, valid) if valid => middle(pages)}
      .sum
  }

  def middle(pages: Pages): Int = {
    pages((pages.length - 1) / 2)
  }

  def correctAndSumInvalidUpdates(input: (RuleBook, Updates)) = {
    val (rules, _) = input

    def pre(e: (Pages, Pages)): Boolean = {
      val (pages, remaining) = e
      val baddies = remaining.flatMap(r => rules.getOrElse(r, Set.empty[Int]))
      !pages.exists(baddies.contains)
    }

    @tailrec
    def pppp(candidates: List[(Pages, Pages)]): List[Pages] = {

      val next: List[(Pages, Pages)] = (for {
          (candidate, remaining) <- candidates
          r <- remaining
        } yield (candidate :+ r, remaining.filterNot(_ == r))).filter{ case (pages, _) => validateOne(rules, pages)._2}.filter(pre)

      next.headOption match {
          case Some(value) if value._2.isEmpty => next.map(_._1)
          case Some(_) => pppp(next)
          case None => Nil
        }
      }
    validateAll(input)
      .collect{ case (pages, valid) if !valid => pages}
      .flatMap(pages => pppp(List((Nil, pages))))
      .map(r => middle(r))
      .sum
  }



  assert(sumMiddlePagesofValidUpdates(parseInput(testInput)) == 143)
  val part1 = run { sumMiddlePagesofValidUpdates(parseInput(providedInput)) }
  println(s"Part 1: $part1")

  assert(correctAndSumInvalidUpdates(parseInput(testInput)) == 123)
  val part2 = run { correctAndSumInvalidUpdates(parseInput(providedInput)) }
  println(s"Part 2: $part2")

}
