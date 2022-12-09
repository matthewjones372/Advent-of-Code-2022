package day8

import zio.*

object Day8 extends ZIOAppDefault:
  val data = Chunk.fromIterator(io.Source.fromResource("day8_input.txt").getLines())
  val grid = Grid(data)

  val run = (for {
    s1 <- grid.visibleTrees.timed
    s2 <- grid.maxView.timed
    _  <- Console.printLine(s"Solution 1: ${s1._2} - time: ${s1._1.toMillis}ms")
    _  <- Console.printLine(s"Solution 2: ${s2._2} - time: ${s2._1.toMillis}ms")
  } yield (s1, s2))
