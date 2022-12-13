package day13

import day13.Packet.*
import zio.*
import zio.prelude.*
import zio.prelude.Ord.*

import scala.io.Source
import scala.util.parsing.combinator.*

enum Packet:
  case NumberPacket(value: Int)
  case SeqPacket(seq: Chunk[Packet])
export Packet.*

object Packet:
  given ordering: Ord[Packet] = Ord.make {
    case (NumberPacket(a), NumberPacket(b)) =>
      a =?= b
    case (l: NumberPacket, r: SeqPacket) =>
      SeqPacket(Chunk(l)) =?= r
    case (l: SeqPacket, r: NumberPacket) =>
      l =?= SeqPacket(Chunk(r))
    case (SeqPacket(l), SeqPacket(r)) =>
      l =?= r
  }

  given scala.Ordering[Packet] = ordering.toScala

  private[Packet] object PacketParser extends RegexParsers:
    private def numberPacket                 = """\d+""".r ^^ { s => NumberPacket(s.toInt) }
    private def seqPacket                    = "[" ~> repsep(packetParser, ",") <~ "]" ^^ (l => SeqPacket(Chunk.from(l)))
    private def packetParser: Parser[Packet] = numberPacket | seqPacket

    def parse(line: String): Packet =
      parseAll(packetParser, line)
        .getOrElse(throw RuntimeException(s"Failed to parse $line"))

  def dividerPacketOf(value: Int): Packet =
    SeqPacket(Chunk(SeqPacket(Chunk(NumberPacket(value)))))
  def from(packetLines: Chunk[String]): Chunk[Packet] =
    packetLines
      .filterNot(_.isBlank)
      .map(_.trim)
      .map(PacketParser.parse)

@main def main =
  val packetStream = Chunk
    .fromIterator(Source.fromResource("day13_input.txt").getLines())

  val packets = Packet.from(packetStream)

  def solution1 =
    packets
      .grouped(2)
      .zipWithIndex
      .collect {
        case (Seq(left, right), i) if left <= right => i + 1
        case _                                      => 0
      }
      .sum

  def decodeSignal(packets: Chunk[Packet], withDividerPackets: Chunk[Packet]): Int =
    val ordered = (packets ++ withDividerPackets).sorted
    withDividerPackets.map(ordered.indexOf(_) + 1).product

  val solution2 =
    decodeSignal(packets = packets, withDividerPackets = Chunk(dividerPacketOf(2), dividerPacketOf(6)))

  println(s"Part 1: $solution1")
  println(s"Part 2: $solution2")
