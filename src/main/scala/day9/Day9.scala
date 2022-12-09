package day9

import scala.io.Source
import scala.util.Using

@main def main =
  val lines     = Using.resource(Source.fromResource("day9_input.txt"))(_.getLines().toList)
  val bridge    = Bridge(lines)
  val solution1 = bridge.tailVisitsWith(knots = 2)
  val solution2 = bridge.tailVisitsWith(knots = 10)

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
