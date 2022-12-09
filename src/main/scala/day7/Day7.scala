package day7

import scala.io.Source

@main def main =
  val lines =
    Source.fromResource("day7_input.txt").getLines().toList

  val fileSystem = FileSystem.from(lines)
  val dirSizes   = fileSystem.dirSizes
  val used       = dirSizes.max
  val needed     = 300_00000 - (700_00000 - used)

  val solution1 = dirSizes.filter(_ < 100_000).sum
  val solution2 = dirSizes.filter(_ > needed).min

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
