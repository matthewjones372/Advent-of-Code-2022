package day2

import scala.io.Source
import scala.util.Using

@main def main =
  val rounds = Using.resource(
    Source
      .fromResource("day2_input.txt")
  ) {
    _.getLines()
      .map(_.split(" "))
      .toList
  }

  val solution1 = rounds.map { case Array(theirCode, yourCode) =>
    Score.ruleSet1(theirCode, yourCode)
  }.sum

  val solution2 = rounds.map { case Array(theirCode, outcomeCode) =>
    Score.ruleSet2(theirCode, outcomeCode)
  }.sum

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
