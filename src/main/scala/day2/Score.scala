package day2

import Play.*
import Outcome.*

object Score:
  def ruleSet1(theirCode: String, yourCode: String): Option[Int] = for {
    theirPlay <- Play.from(theirCode)
    yourPlay  <- Play.from(yourCode)
  } yield {
    yourPlay.points + outcomeFrom(yourPlay, theirPlay).points
  }

  def ruleSet2(theirCode: String, outcomeCode: String): Option[Int] =
    for {
      theirPlay      <- Play.from(theirCode)
      desiredOutcome <- Outcome.from(outcomeCode)
    } yield {
      playFrom(desiredOutCome = desiredOutcome, theirPlay = theirPlay).points + desiredOutcome.points
    }

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

  private def playFrom(desiredOutCome: Outcome, theirPlay: Play): Play =
    (theirPlay, desiredOutCome) match
      case (Rock, Win)      => Paper
      case (Rock, Draw)     => Rock
      case (Rock, Lose)     => Scissors
      case (Paper, Win)     => Scissors
      case (Paper, Draw)    => Paper
      case (Paper, Lose)    => Rock
      case (Scissors, Win)  => Rock
      case (Scissors, Draw) => Scissors
      case (Scissors, Lose) => Paper
