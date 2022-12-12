package day12

import zio.Chunk

import scala.annotation.tailrec
import scala.io.Source

final case class Point(x: Int, y: Int)
type Points = Map[Point, Char]

final class Grid(points: Points):
  def bfs(start: Point, target: Point) =
    @tailrec
    def loop(remaining: Chunk[Point], costMap: Map[Point, Int]): Int =
      if remaining.isEmpty then -1
      else {
        val point = remaining.head
        if point == target then costMap(point)
        else {
          val (moreRemaining, newCostMap) =
            neighbors(point).filter(points.contains).foldLeft((remaining, costMap)) {
              case ((remaining, costMap), neighbor) =>
                if isBlockedBy(point, neighbor) && !costMap.contains(neighbor) then {
                  (remaining :+ neighbor, costMap.updated(neighbor, costMap(point) + 1))
                } else (remaining, costMap)
            }

          loop(moreRemaining.tail, newCostMap)
        }
      }

    loop(remaining = Chunk(start), costMap = Map(start -> 0))

  private def isBlockedBy(current: Point, other: Point): Boolean =
    def elevation(point: Point) = points(point) match
      case 'S'   => 'a'
      case 'E'   => 'z'
      case other => other

    elevation(current) - elevation(other) <= 1

  private val width  = points.keys.map(_.x).max + 1
  private val height = points.keys.map(_.y).max + 1

  private def neighbors(point: Point): Chunk[Point] =
    val Point(x, y) = point
    Chunk(
      Point(x - 1, y),
      Point(x + 1, y),
      Point(x, y - 1),
      Point(x, y + 1)
    ).filter(inBounds)

  private def inBounds(point: Point): Boolean =
    point.x >= 0 && point.x < width && point.y >= 0 && point.y < height

object PointParser:
  def parse(input: Chunk[String]): Points =
    input.indices.flatMap { y =>
      input.head.indices.map { x =>
        Point(x, y) -> input(y)(x)
      }
    }.toMap

@main def main =
  val data      = Chunk.fromIterator(Source.fromResource("day12_input.txt").getLines())
  val points    = PointParser.parse(data)
  val grid      = Grid(points)
  val start     = points.map(_.swap)('E')
  val solution1 = grid.bfs(start, points.map(_.swap)('S'))
  val solution2 = grid.bfs(start, points.map(_.swap)('a'))

  println(s"${Console.RED} Part 1:${Console.GREEN} $solution1${Console.RESET}")
  println(s"${Console.RED} Part 2:${Console.GREEN} $solution2${Console.RESET}")
