package day1

import scala.io.Source


object Day1 extends App:
  val elfWeights = Source
    .fromResource("day1_input.txt")
    .getLines()
    .map(_.trim)
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).sum)


  val solution1 = elfWeights.max
  val solution2 = elfWeights.sorted(Ordering.Int.reverse).take(3).sum

  println(s"Part 1: $solution1")
  println(s"Part 2: $solution2")