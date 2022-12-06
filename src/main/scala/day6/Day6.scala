package day6

import scala.io.Source
import scala.util.Using

@main def main() =
  def dataStream: Iterator[Char] =
    Source.fromResource("day6_input.txt").iterator

  extension (dataStream: Iterator[Char])
    def findIndicatorsWith(offset: Int): Int =
      val (_, index) = dataStream.zipWithIndex
        .sliding(offset)
        .filter(
          _.map { case (c, _) => c }.distinct.size == offset
        )
        .next()
        .last

      index + 1

  val solution1 = dataStream.findIndicatorsWith(offset = 4)
  val solution2 = dataStream.findIndicatorsWith(offset = 14)

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
