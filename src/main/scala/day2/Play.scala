package day2

enum Play(val points: Int):
  case Rock extends Play(1)
  case Paper extends Play(2)
  case Scissors extends Play(3)

object Play:
  private val playMapping = Map(
    "A" -> Rock,
    "B" -> Paper,
    "C" -> Scissors,
    "X" -> Rock,
    "Y" -> Paper,
    "Z" -> Scissors
  )
  def from(code: String): Option[Play] =
    playMapping.get(code)
