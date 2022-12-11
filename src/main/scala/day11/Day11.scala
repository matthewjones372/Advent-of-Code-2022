package day11

import zio.*
import zio.nio.file.*

object Day11 extends ZIOAppDefault:
  def run = (for {
    lines  <- Files.readAllLines(Path("src/main/resources/day11_input.txt")).map(Chunk.from)
    monkeys = MonkeyParser.parse(lines)
    s1     <- CrazyMonkeys.withFirstStrategy(monkeys).simulate(20).timed
    s2     <- CrazyMonkeys.withSecondStrategy(monkeys).simulate(100_00).timed
    _      <- Console.printLine(s"Solution 1: ${s1._2}\t\t\t\ttime ${s1._1.toMillis}ms")
    _      <- Console.printLine(s"Solution 2: ${s2._2}\t\t\ttime ${s2._1.toMillis}ms")
  } yield (s1, s2)).timed.tap { case (time, (_, _)) => Console.printLine(s"Total Time - ${time.toMillis}ms") }
