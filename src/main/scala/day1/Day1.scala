package day1

import scala.io.Source
import scala.util.Using

extension (seq: Seq[Int])
  def top(n: Int): Seq[Int] =
    seq.sorted.takeRight(n)

@main def main =
  val elfWeights = Using.resource(Source.fromResource("day1_input.txt"))(
    _.getLines()
      .mkString("\n")
      .split("\n\n")
      .map(_.split("\n").map(_.toInt).sum)
  )

  val solution1 = elfWeights.max
  val solution2 = elfWeights.top(n = 3).sum

  println(s"Part 1: $solution1")
  println(s"Part 2: $solution2")
