package day5

import scala.io.Source
import scala.util.Using

@main def main =

  def cratesFrom(text: Seq[String]) =
    val columns =
      text.map(_.padTo(text.map(_.length).max, ' ')).reverse.transpose

    (1 until columns.size by 4)
      .map(i => columns(i).tail.filter(_ != ' ').reverse)

  val rawCrates = Using.resource(
    Source.fromResource("day5_crates.txt")
  ) {
    _.getLines().toList
  }

  val crates = cratesFrom(
    rawCrates
  )

  val instructions = Using.resource(Source.fromResource("day5_instructions.txt")) {
    _.getLines().map { case s"move ${moves} from ${from} to ${to}" =>
      (moves.toInt, from.toInt - 1, to.toInt - 1)
    }.toList
  }

  val solution1 = instructions
    .foldLeft(crates) { case (crates, (moves, from, to)) =>
      val (moved, movedFrom) = crates(from).splitAt(moves)
      crates
        .updated(to, moved.reverse ++ crates(to))
        .updated(from, movedFrom)
    }
    .map(_.head)
    .mkString

  val solution2 = instructions
    .foldLeft(crates) { case (crates, (moves, from, to)) =>
      val (moved, movedFrom) = crates(from).splitAt(moves)
      crates
        .updated(to, moved ++ crates(to))
        .updated(from, movedFrom)
    }
    .map(_.head)
    .mkString

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
