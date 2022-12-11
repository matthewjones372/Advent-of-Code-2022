package day9

final class Bridge(lines: List[String]):
  def tailVisitsWith(knots: Int): Int =
    moves
      .scanLeft(List.fill(knots)(Position.zero)) {
        case ((head :: tail), point) =>
          tail.scanLeft(head + point) { case (prev, curr) =>
            if (prev.isTouching(curr)) curr else prev.changeBetween(curr)
          }

        case (acc, _) => acc
      }
      .map(_.last)
      .distinct
      .length

  private val moves: List[Position] =
    def from(direction: String) =
      direction match
        case "L" => Position(-1, 0)
        case "R" => Position(1, 0)
        case "U" => Position(0, -1)
        case "D" => Position(0, 1)

    lines.flatMap { case s"$direction $count" =>
      List.fill(count.toInt)(from(direction))
    }

object Bridge:
  def apply(lines: List[String]): Bridge = new Bridge(lines)

final case class Position(x: Int, y: Int):
  def isTouching(that: Position): Boolean =
    (this.x - that.x).abs <= 1 && (this.y - that.y).abs <= 1

  def +(that: Position): Position =
    Position(this.x + that.x, this.y + that.y)

  def changeBetween(that: Position): Position =
    (Position((this.x - that.x).sign, (this.y - that.y).sign)) + that

object Position:
  def zero: Position = Position(0, 0)
