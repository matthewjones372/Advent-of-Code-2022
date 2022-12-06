package day6

import scala.io.Source
import scala.util.Using

@main def main() =
  def dataStream: Iterator[Char] =
    Source.fromResource("day6_input.txt").iterator

  def find(offset: Int, dataStream: Iterator[Char]): Int = {
    val (_, index) = dataStream.zipWithIndex
      .sliding(offset)
      .filter(
        _.map { case (c, _) => c }.distinct.size == offset
      )
      .next()
      .last

    index + 1
  }

  println(s"${Console.RED} Part 1:${Console.GREEN} ${find(offset = 4, dataStream = dataStream)}${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} ${find(offset = 14, dataStream = dataStream)}${Console.RESET}")
