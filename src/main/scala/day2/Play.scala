package day2

enum Play(val points: Int):
  case Rock extends Play(1)
  case Paper extends Play(2)
  case Scissors extends Play(3)

object Play:
  def of(code: String): Play =
    code match
      case "A" => Rock
      case "B" => Paper
      case "C" => Scissors
      case "X" => Rock
      case "Y" => Paper
      case "Z" => Scissors
