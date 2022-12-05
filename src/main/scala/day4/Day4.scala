package day4

import scala.io.Source
import scala.util.Using

@main def main =
  def range: PartialFunction[String, Range] =
    case s"$lower-$upper" => lower.toInt to upper.toInt

  val sections = Using.resource(
    Source
      .fromResource("day4_input.txt")
  ) {
    _.getLines().map { case s"${range(l)},${range(r)}" =>
      (l, r)
    }.toList
  }

  val solution1 = sections.count { case (l, r) =>
    l.containsSlice(r) || r.containsSlice(l)
  }

  val solution2 = sections.count { case (l, r) =>
    (l intersect r).nonEmpty
  }

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
