package day2

import Play.*
import Outcome.*

object Score:
  def ruleSet1(theirCode: String, yourCode: String): Int =
    val yourPlay  = Play.of(yourCode)
    val theirPlay = Play.of(theirCode)
    yourPlay.points + outcomeFrom(yourPlay, theirPlay).points

  def ruleSet2(theirCode: String, outcomeCode: String): Int =
    val desiredOutcome = Outcome.of(outcomeCode)
    val theirPlay      = Play.of(theirCode)
    playFrom(desiredOutcome, theirPlay).points + Outcome.of(outcomeCode).points

  private def outcomeFrom(yourPlay: Play, theirPlay: Play): Outcome =
    (yourPlay, theirPlay) match
      case (Rock, Rock)         => Draw
      case (Rock, Paper)        => Lose
      case (Rock, Scissors)     => Win
      case (Paper, Rock)        => Win
      case (Paper, Paper)       => Draw
      case (Paper, Scissors)    => Lose
      case (Scissors, Scissors) => Draw
      case (Scissors, Paper)    => Win
      case (Scissors, Rock)     => Lose

  private def playFrom(desiredOutcome: Outcome, theirPlay: Play): Play =
    (theirPlay, desiredOutcome) match
      case (Rock, Win)      => Paper
      case (Rock, Draw)     => Rock
      case (Rock, Lose)     => Scissors
      case (Paper, Win)     => Scissors
      case (Paper, Draw)    => Paper
      case (Paper, Lose)    => Rock
      case (Scissors, Win)  => Rock
      case (Scissors, Draw) => Scissors
      case (Scissors, Lose) => Paper
