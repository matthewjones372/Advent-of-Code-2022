package day10

import zio.*

import scala.io.Source
import scala.util.Using

sealed trait Instruction
object Instruction:
  case object NOOP                  extends Instruction
  final case class AddX(value: Int) extends Instruction

  def from(instruction: String): Instruction =
    instruction match
      case s"noop"        => NOOP
      case s"addx $value" => AddX(value = value.toInt)

object Day10 extends ZIOAppDefault:
  val instructions =
    Using.resource(Source.fromResource("day10_input.txt")) { l =>
      Chunk.fromIterator(l.getLines().map(Instruction.from))
    }

  def countInstructions: UIO[Ref[Chunk[Int]]] =
    Ref.make(Chunk(1)).flatMap { ref =>
      ZIO
        .foreach(instructions) {
          case Instruction.NOOP =>
            ref.getAndUpdate(acc => acc :+ acc.last)
          case Instruction.AddX(value) =>
            ref.getAndUpdate(acc => acc ++: Chunk(acc.last, acc.last + value))
        }
        .as(ref)
    }

  def countRegisters(ref: Ref[Chunk[Int]]) =
    ref.get.map {
      _.zipWithIndex.map { case (value, index) => (value, index + 1) }.collect {
        case (register, cycle) if (cycle - 20) % 40 == 0 =>
          register * cycle
      }.sum
    }

  def decode(ref: Ref[Chunk[Int]]) =
    ref.get.map(
      _.grouped(40)
        .map(_.zipWithIndex.map { case (column, index) =>
          if (index - column).abs <= 1 then "🎅" else "🎄"
        }.mkString("\t"))
        .mkString("\n")
    )

  def run = (for {
    ref <- countInstructions
    r1  <- countRegisters(ref)
    r2  <- decode(ref)
    _   <- Console.printLine(s"Solution 1: $r1")
    _   <- Console.printLine("Solution 2")
    _   <- Console.printLine(r2)
  } yield (r1, r2)).timed.tap { case (time, (_, _)) => Console.printLine(s"${time}ms") }
