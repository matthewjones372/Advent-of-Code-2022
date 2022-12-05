package day2

import scala.io.Source
import scala.util.Using

@main def main =
  val rounds = Using.resource(
    Source
      .fromResource("day2_input.txt")
  ) {
    _.getLines().map { case s"$l $r" => (l, r) }.toList
  }

  val solution1 = rounds.map { case (theirCode, yourCode) =>
    Score.ruleSet1(theirCode, yourCode)
  }.sum

  val solution2 = rounds.map { case (theirCode, outcomeCode) =>
    Score.ruleSet2(theirCode, outcomeCode)
  }.sum

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
