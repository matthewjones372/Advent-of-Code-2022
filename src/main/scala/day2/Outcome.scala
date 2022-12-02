package day2

enum Outcome(val points: Int):
  case Win extends Outcome(6)
  case Draw extends Outcome(3)
  case Lose extends Outcome(0)

object Outcome:
  private val outcomeMapping = Map(
    "X" -> Lose,
    "Y" -> Draw,
    "Z" -> Win
  )

  def from(code: String): Option[Outcome] =
    outcomeMapping.get(code)
