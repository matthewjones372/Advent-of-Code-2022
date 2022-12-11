package day11

import zio.*

final class CrazyMonkeys(
  initialState: Monkeys,
  withStrategy: AdjustmentStrategy = Monkeys => Monkeys
):
  def simulate(rounds: Int): UIO[Long] =
    Ref.make(withStrategy(initialState)).flatMap { ref =>
      ZIO.foreach(1 to rounds)(_ => update(ref)) *> activeProduct(ref)
    }

  private def activeProduct(ref: Ref[Monkeys]): UIO[Long] =
    ref.get.map(_.map(_.seen).sorted.takeRight(2).product)

  private def update(ref: Ref[Monkeys]): UIO[Monkeys] =
    ref.getAndUpdate { state =>
      state.indices.foldLeft(state) { case (state, id) =>
        val currentMonkey      = state(id)
        val (toIds, takingIds) = currentMonkey.partitionItems

        state
          .clearItems(id)
          .giveFrom(currentMonkey, to = toIds)
          .takeFrom(currentMonkey, other = takingIds)
      }
    }

object CrazyMonkeys:
  private val firstStrategy = (state: Monkeys) => state.map(monkey => monkey.copy(operation = monkey.operation(_) / 3))

  private val secondStrategy = (state: Monkeys) =>
    val productTest = state.map(_.condition).product
    state.map(monkey => monkey.copy(operation = monkey.operation(_) % productTest))

  def withFirstStrategy(monkeys: Monkeys): CrazyMonkeys =
    CrazyMonkeys(initialState = monkeys, withStrategy = firstStrategy)

  def withSecondStrategy(monkeys: Monkeys): CrazyMonkeys =
    CrazyMonkeys(initialState = monkeys, withStrategy = secondStrategy)
