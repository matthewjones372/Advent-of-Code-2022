package day8

import zio.*
import zio.nio.file.*

import scala.Console as SConsole
import scala.Console.*

object Day8 extends ZIOAppDefault:
  val run = for {
    input <- Files.readAllLines(Path("src/main/resources/day8_input.txt"))
    grid   = Grid(input)
    s1    <- grid.visibleTrees.timed
    s2    <- grid.maxView.timed
    _ <-
      Console.printLine(
        s"$RED Solution 1:$GREEN ${s1._2}\t$BLUE Execution Time: $YELLOW${s1._1.toMillis}ms$RESET"
      )
    _ <-
      Console.printLine(
        s"$RED Solution 2:$GREEN ${s2._2}\t$BLUE Execution Time: $YELLOW${s2._1.toMillis}ms$RESET"
      )
  } yield ()
