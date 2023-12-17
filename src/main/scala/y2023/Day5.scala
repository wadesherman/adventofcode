package y2023

import util.Utils.lazyList
import util.ProvidedInput

object Day5 extends App with ProvidedInput {
  val year = 2023
  val day = 5
  val testInput = Array(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  )

  case class Range(
      sourceStart: BigInt,
      sourceEnd: BigInt,
      delta: BigInt
  )

  object Range {
    def apply(d: BigInt, s: BigInt, r: BigInt): Range = {
      new Range(
        sourceStart = s,
        sourceEnd = s + r - 1,
        delta = d - s
      )
    }
  }

  case class Almanac(
      S2S: Seq[Range] = Nil,
      S2F: Seq[Range] = Nil,
      F2W: Seq[Range] = Nil,
      W2L: Seq[Range] = Nil,
      L2T: Seq[Range] = Nil,
      T2H: Seq[Range] = Nil,
      H2L: Seq[Range] = Nil
  ) {
    def all: Seq[Seq[Range]] =
      Seq(this.S2S, this.S2F, this.F2W, this.W2L, this.L2T, this.T2H, this.H2L)
  }

  case class Seeds(
      seeds: Seq[BigInt]
  )

  val lookup: (Seq[Range], BigInt) => BigInt = { case (map, k) =>
    map.find(r =>
      k >= r.sourceStart &&
        k <= r.sourceEnd
    ) match {
      case Some(range) => k + range.delta
      case None        => k
    }
  }

  val reverseLookup: (Seq[Range], BigInt) => BigInt = { case (map, k) =>
    map.find(r =>
      k >= r.sourceStart + r.delta &&
        k <= r.sourceEnd + r.delta
    ) match {
      case Some(range) => k - range.delta
      case None        => k
    }
  }

  val seedToLocation: Almanac => BigInt => BigInt = { a =>
    a.all
      .map(r => lookup(r, _))
      .reduceLeft(_ andThen _)
  }

  val locationToSeed: Almanac => BigInt => BigInt = { a =>
    a.all.reverse
      .map(r => reverseLookup(r, _))
      .reduceLeft(_ andThen _)
  }

  def appendAndSort(s: Seq[Range], r: Range): Seq[Range] =
    (s :+ r).sortBy(_.sourceStart)

  def parseInput(input: Array[String]): (Seeds, Almanac) = {
    val seedList: Seq[BigInt] =
      input(0).replace("seeds: ", "").split(" ").map(s => BigInt(s)).toSeq

    val almanac: Almanac = input
      .foldLeft(("", Almanac())) { case ((label, almanac), row) =>
        if (row.contains("map")) {
          (row, almanac)
        } else if (label != "" && row.exists(_.isDigit)) {
          val d = row.split(" ").map(s => BigInt(s))
          label match {
            case "seed-to-soil map:" =>
              (
                label,
                almanac.copy(S2S = appendAndSort(almanac.S2S, Range(d(0), d(1), d(2))))
              )
            case "soil-to-fertilizer map:" =>
              (
                label,
                almanac.copy(S2F = appendAndSort(almanac.S2F, Range(d(0), d(1), d(2))))
              )
            case "fertilizer-to-water map:" =>
              (
                label,
                almanac.copy(F2W = appendAndSort(almanac.F2W, Range(d(0), d(1), d(2))))
              )
            case "water-to-light map:" =>
              (
                label,
                almanac.copy(W2L = appendAndSort(almanac.W2L, Range(d(0), d(1), d(2))))
              )
            case "light-to-temperature map:" =>
              (
                label,
                almanac.copy(L2T = appendAndSort(almanac.L2T, Range(d(0), d(1), d(2))))
              )
            case "temperature-to-humidity map:" =>
              (
                label,
                almanac.copy(T2H = appendAndSort(almanac.T2H, Range(d(0), d(1), d(2))))
              )
            case "humidity-to-location map:" =>
              (
                label,
                almanac.copy(H2L = appendAndSort(almanac.H2L, Range(d(0), d(1), d(2))))
              )
          }
        } else {
          (label, almanac)
        }
      }
      ._2

    (Seeds(seeds = seedList), almanac)
  }

  def part2Seeds(seeds: Seeds): Seq[(BigInt, BigInt)] = {
    seeds.seeds.sliding(2, 2).map(l => (l(0), l(1))).toSeq
  }

  def part2SeedRanges(seeds: Seeds): Seq[Range] = {
    part2Seeds(seeds).map(l => Range(l._1, l._1, l._2)).sortBy(_.sourceStart)
  }

  def part1Score(seeds: Seeds, seedToLocation: BigInt => BigInt): BigInt = {
    seeds.seeds
      .map(seed => seedToLocation(seed))
      .min
  }

  // Just searches locations from 0 until it finds a valid seed.
  def part2BruteForce(
      seeds: Seeds,
      locationToSeed: BigInt => BigInt
  ): BigInt = {
    val p2seeds = part2Seeds(seeds)
    lazyList
      .find(l => {
        val seed = locationToSeed(l)
        p2seeds.exists { case (start, count) =>
          seed >= start && seed <= start + count - 1
        }
      })
      .get
  }

  def sliceAndMap(data: Seq[Range], mapper: Seq[Range]): Seq[Range] = {
    if (data == Nil) {
      Nil
    } else if (mapper == Nil) {
      data
    } else {
      val dh :: dt = data
      val mh :: mt = mapper

      // ###
      //     #####
      // data fully before mapper
      if (dh.sourceEnd < mh.sourceStart) {
        dh +: sliceAndMap(dt, mapper)

        //      ####
        //  ###
        //mapper fully before data
      } else if (mh.sourceEnd < dh.sourceStart) {
        sliceAndMap(data, mt)

        // ####     ########
        //   ####      ###
        //data lowest
      } else if (dh.sourceStart < mh.sourceStart) {
        val leftSlice = dh.copy(sourceEnd = mh.sourceStart - 1)
        val rightSlice = dh.copy(sourceStart = mh.sourceStart)
        leftSlice +: sliceAndMap(rightSlice +: dt, mapper)

        //     #        ########
        //   ####      ###
        // mapper lowest
      } else if (mh.sourceStart < dh.sourceStart) {
        val rightSlice = mh.copy(sourceStart = dh.sourceStart)
        sliceAndMap(data, rightSlice +: mt)

        //  ##     ########    #####
        //  ####   ###         #####
        // even start
      } else {
        // data ends first or even
        if (dh.sourceEnd <= mh.sourceEnd) {
          val adjustedRange = dh.copy(
            sourceStart = dh.sourceStart + mh.delta,
            sourceEnd = dh.sourceEnd + mh.delta
          )
          adjustedRange +: sliceAndMap(dt, mapper)

          //mapper ends first
        } else {
          val leftSlice = dh.copy(
            sourceStart = dh.sourceStart + mh.delta,
            sourceEnd = mh.sourceEnd + mh.delta
          )
          val rightSlice = dh.copy(sourceStart = mh.sourceEnd + 1)
          leftSlice +: sliceAndMap(rightSlice +: dt, mt)
        }
      }
    }

  }

  def part2Score(seeds: Seeds, almanac: Almanac): BigInt = {
    val seedRanges = part2SeedRanges(seeds).sortBy(_.sourceStart)
    val mappingRanges = almanac.all

    mappingRanges
      .foldLeft(seedRanges) {
        case (mapped, mapper) => {
          sliceAndMap(mapped.sortBy(_.sourceStart), mapper)
        }
      }
      .map(_.sourceStart)
      .min
  }

  val (testSeeds, testAlmanac) = parseInput(testInput)
  val testSeedToLocation = seedToLocation(testAlmanac)
  val testLocationToSeed = locationToSeed(testAlmanac)

  assert(part1Score(testSeeds, testSeedToLocation) == 35)
  assert(part2BruteForce(testSeeds, testLocationToSeed) == 46)
  assert(part2Score(testSeeds, testAlmanac) == 46)

  val (dataSeeds, dataAlmanac) = parseInput(providedInput)
  val dataSeedToLocation = seedToLocation(dataAlmanac)
  val dataLocationToSeed = locationToSeed(dataAlmanac)

  val part1 = part1Score(dataSeeds, dataSeedToLocation)
  println(s"Part 1: $part1")

  val part2 = run {part2Score(dataSeeds, dataAlmanac) }
  println(s"Part 2: $part2")

  val part2brute = run { part2BruteForce(dataSeeds, dataLocationToSeed) }
  println(s"Part 2 Brute Force: $part2brute")

}
