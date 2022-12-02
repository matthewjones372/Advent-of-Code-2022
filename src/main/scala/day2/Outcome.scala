package day2

enum Outcome(val points: Int):
  case Win extends Outcome(6)
  case Draw extends Outcome(3)
  case Lose extends Outcome(0)

object Outcome:
  def of(code: String): Outcome =
    code match
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
