package day11

import zio.*

final case class Monkey(
  items: Chunk[Long],
  operation: Long => Long,
  condition: Int,
  truthyId: Int,
  falselyId: Int,
  seen: Long
):
  def clear: Monkey =
    copy(items = Chunk.empty, seen = seen + items.size)

  def partitionItems =
    items.map(operation).partition(_ % condition == 0)

  def addItems(thatItems: Chunk[Long]): Monkey =
    copy(items = items ++ thatItems)

type Monkeys            = Chunk[Monkey]
type AdjustmentStrategy = Monkeys => Monkeys

extension (monkeys: Monkeys)
  def clearItems(monkeyId: Int): Monkeys =
    monkeys.updated(monkeyId, monkeys(monkeyId).clear)

  def giveFrom(currentMonkey: Monkey, to: Chunk[Long]): Monkeys =
    monkeys.updated(currentMonkey.truthyId, monkeys(currentMonkey.truthyId).addItems(to))

  def takeFrom(currentMonkey: Monkey, other: Chunk[Long]): Monkeys =
    monkeys.updated(currentMonkey.falselyId, monkeys(currentMonkey.falselyId).addItems(other))
