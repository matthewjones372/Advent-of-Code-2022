package day10

import zio.Chunk

import scala.io.Source
import scala.util.Using
import zio.*
import scala.Console as SConsole

sealed trait Instruction
object Instruction {
  case object NOOP                  extends Instruction
  final case class AddX(value: Int) extends Instruction

  def from(instruction: String): Instruction =
    instruction match
      case s"noop"        => NOOP
      case s"addx $value" => AddX(value = value.toInt)
}

object Day10 extends ZIOAppDefault:
  val instructions =
    Using.resource(Source.fromResource("day10_input.txt")) { l =>
      Chunk.fromIterator(l.getLines().map(Instruction.from))
    }

  def countInstructions(ref: Ref[Chunk[Int]]) =
    ZIO.foreach(instructions) {
      case Instruction.NOOP =>
        ref.getAndUpdate(acc => acc :+ acc.last)
      case Instruction.AddX(value) =>
        ref.getAndUpdate(acc => acc ++: Chunk(acc.last, acc.last + value))
    }

  def solution1(ref: Ref[Chunk[Int]]) =
    ref.get.map {
      _.zipWithIndex.collect {
        case (register, cycle) if (cycle - 19) % 40 == 0 =>
          register * (cycle + 1)
      }.sum
    }

  def solution2(ref: Ref[Chunk[Int]]) = ref.get.map(
    _.grouped(40)
      .map(_.zipWithIndex.map { case (column, index) =>
        if (index - column).abs <= 1 then s"${SConsole.GREEN}🎅" else s"${SConsole.GREEN}#"
      }.mkString("\t"))
      .mkString("\n")
  )

  def run = (for {
    ref <- Ref.make(Chunk(1))
    _   <- countInstructions(ref)
    r1  <- solution1(ref)
    r2  <- solution2(ref)
    _   <- Console.printLine(s"Solution 1: $r1")
    _   <- Console.printLine("Solution 2")
    _   <- Console.printLine(r2)
  } yield r1)
