package day8

import zio.*
import scala.Console as SConsole
import SConsole.*

object Day8 extends ZIOAppDefault:
  val data = Chunk.fromIterator(io.Source.fromResource("day8_input.txt").getLines())
  val grid = Grid(data)

  val run = for {
    s1 <- grid.visibleTrees.timed
    s2 <- grid.maxView.timed
    _ <-
      Console.printLine(
        s"$RED Solution 1:$GREEN ${s1._2}\t$BLUE Execution Time: $YELLOW${s1._1.toMillis}ms$RESET"
      )
    _ <-
      Console.printLine(
        s"$RED Solution 2:$GREEN ${s2._2}\t$BLUE Execution Time: $YELLOW${s2._1.toMillis}ms$RESET"
      )
  } yield ()
