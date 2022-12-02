package day2

import scala.io.Source

@main def main =
  val rounds = Source
    .fromResource("day2_input.txt")
    .getLines()
    .map(_.split(" "))
    .toList

  val solution1 = rounds.flatMap { case Array(theirCode, yourCode) =>
    Score.ruleSet1(theirCode, yourCode)
  }.sum

  val solution2 = rounds.flatMap { case Array(theirCode, outcomeCode) =>
    Score.ruleSet2(theirCode, outcomeCode)
  }.sum

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
