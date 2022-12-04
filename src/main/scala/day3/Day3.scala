package day3

import scala.io.Source
import scala.util.Using

object Priority:
  private val priorities =
    (('a' to 'z').zipWithIndex.map { case (c, i) => (c, i + 1) } ++
      ('A' to 'Z').zipWithIndex.map { case (c, i) => (c, i + 27) }).toMap

  def of(char: Char): Int = priorities.getOrElse(char, 0)

@main def main =
  val sacks = Using.resource(
    Source
      .fromResource("day3_input.txt")
  ) {
    _.getLines().toList
  }

  val solution1 = sacks.flatMap { sack =>
    val (left, right) = sack.splitAt(sack.length / 2)
    (left intersect right).distinct.map(Priority.of)
  }.sum

  val solution2 = sacks
    .grouped(3)
    .flatMap(_.reduce(_ intersect _).headOption.map(Priority.of))
    .sum

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
