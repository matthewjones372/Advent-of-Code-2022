package day11

import zio.*

final class CrazyMonkeys(
  initialMonkeys: Monkeys,
  withStrategy: AdjustmentStrategy = Monkeys => Monkeys
):
  def simulate(rounds: Int): UIO[Long] =
    Ref.make(withStrategy(initialMonkeys)).flatMap { ref =>
      ZIO.foreach(1 to rounds)(_ => update(ref)) *> mostActive(ref)
    }

  private def mostActive(ref: Ref[Monkeys]): UIO[Long] =
    ref.get.map(_.map(_.seen).sorted.takeRight(2).product)

  private def update(ref: Ref[Monkeys]): UIO[Monkeys] =
    ref.getAndUpdate { monkeys =>
      monkeys.indices.foldLeft(monkeys) { case (monkeyState, id) =>
        val currentMonkey    = monkeyState(id)
        val (toIds, fromIds) = currentMonkey.partitionItems

        monkeyState
          .clearItems(id)
          .giveFrom(currentMonkey, to = toIds)
          .takeFrom(currentMonkey, other = fromIds)
      }
    }

object CrazyMonkeys:
  private val firstStrategy = (monkeys: Monkeys) =>
    monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) / 3))

  private val secondStrategy = (monkeys: Monkeys) =>
    val productTest = monkeys.map(_.condition).product
    monkeys.map(monkey => monkey.copy(operation = monkey.operation(_) % productTest))

  def withFirstStrategy(monkeys: Monkeys): CrazyMonkeys =
    CrazyMonkeys(monkeys, firstStrategy)

  def withSecondStrategy(monkeys: Monkeys): CrazyMonkeys =
    CrazyMonkeys(monkeys, secondStrategy)
