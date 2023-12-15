package y2023

import util.ProvidedInput

object Day7 extends App with ProvidedInput {
  val year = 2023
  val day = 7

  val testInput = Array(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )

  case class Ranking(
      rankedCards: (Int, Int, Int, Int, Int),
      handRank: Int
  )
  object Ranking {
    implicit def ordering: Ordering[Ranking] =
      Ordering.by(r => (r.handRank, r.rankedCards))
  }

  case class Hand(
      cards: Seq[String],
      normalScore: Ranking,
      jokerScore: Ranking,
      bid: Int
  )

  object Hand {

    val cardRanks: Map[String, Int] = Seq(
      "A" -> 14,
      "K" -> 13,
      "Q" -> 12,
      "J" -> 11,
      "T" -> 10,
      "9" -> 9,
      "8" -> 8,
      "7" -> 7,
      "6" -> 6,
      "5" -> 5,
      "4" -> 4,
      "3" -> 3,
      "2" -> 2
    ).toMap

    val JOKERVALUE = 0
    val jokerCardRanks = cardRanks.map {
      case (str, _) if str == "J" => str -> JOKERVALUE
      case (str, i)               => str -> i
    }

    val handRanks: Seq[Int] => Int = {
      case 5 :: Nil      => 7
      case 4 :: _        => 6
      case 3 :: 2 :: Nil => 5
      case 3 :: _        => 4
      case 2 :: 2 :: _   => 3
      case 2 :: _        => 2
      case _             => 1
    }

    def rankHand: Seq[Int] => Int = { cardRanks =>
      val rankCounts = cardRanks
        .groupBy(r => r)
        .map { case (r, cards) => r -> cards.length }
        .toList
        .sortBy(_._2)
        .reverse

      val maybeJoker = rankCounts.find(_._1 == JOKERVALUE)
      val noJokers = rankCounts.filter(_._1 != JOKERVALUE)

      val merged = maybeJoker match {
        case Some(joker) =>
          noJokers match {
            case head :: tail => head._1 -> (head._2 + joker._2) :: tail
            case Nil          => joker :: Nil
          }
        case None => rankCounts
      }
      handRanks(merged.map(_._2))
    }

    def toCardTuple(cards: Seq[Int]): (Int, Int, Int, Int, Int) = {
      (cards(0), cards(1), cards(2), cards(3), cards(4))
    }

    def apply(cards: String, bid: Int): Hand = {
      val (normalRankedCards, jokerRankedCards) =
        cards.map(c => cardRanks(c.toString) -> jokerCardRanks(c.toString)).unzip

      Hand(
        cards = cards.toList.map(_.toString),
        normalScore = Ranking(
          rankedCards = toCardTuple(normalRankedCards),
          handRank = rankHand(normalRankedCards)
        ),
        jokerScore = Ranking(
          rankedCards = toCardTuple(jokerRankedCards),
          handRank = rankHand(jokerRankedCards)
        ),
        bid = bid
      )
    }
  }

  def parseInput(input: Array[String]): Array[Hand] = {
    input.map { case s"${cards} ${bid}" => Hand(cards, bid.toInt) }
  }

  def score(hands: Array[Hand], orderBy: Hand => Ranking): Int = {
    hands
      .sortBy(orderBy)
      .zipWithIndex
      .map { case (hand, i) =>
        hand.bid * (i + 1)
      }
      .sum
  }

  def part1Score(hands: Array[Hand]): Int = score(hands, _.normalScore)
  def part2Score(hands: Array[Hand]): Int = score(hands, _.jokerScore)

  val parsedTestInput = parseInput(testInput)
  assert(part1Score(parsedTestInput) == 6440)
  assert(part2Score(parsedTestInput) == 5905)

  val parsedProvidedInput = parseInput(providedInput)
  println(s"Part 1: ${part1Score(parsedProvidedInput)}")
  println(s"Part 2: ${part2Score(parsedProvidedInput)}")

}
