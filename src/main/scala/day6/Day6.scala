package day6

import scala.io.Source
import scala.util.Using

@main def main() =
  val dataStream: Seq[Char] = Using.resource(
    Source.fromResource("day6_input.txt")
  ) {
    _.mkString.toList
  }

  def find(offset: Int, dataStream: Seq[Char]) = {
    val (_, index) = dataStream.zipWithIndex
      .sliding(offset)
      .filter {
        _.map { case (c, _) => c }.distinct.size == offset
      }
      .next()
      .last

    index + 1
  }

  println(s"${Console.RED} Part 1:${Console.GREEN} ${find(offset = 4, dataStream = dataStream)}${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} ${find(offset = 14, dataStream = dataStream)}${Console.RESET}")
