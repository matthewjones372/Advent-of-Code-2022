package day11

import zio.Chunk

object MonkeyParser:
  type Parser[A] = PartialFunction[String, A]

  def parse(monkeys: Chunk[String]): Monkeys =
    val it = monkeys
      .grouped(7)
      .flatMap { ml =>
        val monkeyMap = ml.zipWithIndex.map { case (line, i) => (i, line.trim) }.toMap
        for {
          startingItems <- monkeyMap.get(1).map(itemsParser)
          operation     <- monkeyMap.get(2).map(operationParser)
          testCondition <- monkeyMap.get(3).map(testConditionParser)
          truthOutcome  <- monkeyMap.get(4).map(testOutcomeParser)
          falseOutcome  <- monkeyMap.get(5).map(testOutcomeParser)
        } yield Monkey(
          items = startingItems,
          operation = operation,
          condition = testCondition,
          truthyId = truthOutcome,
          falselyId = falseOutcome,
          seen = 0
        )
      }
    Chunk.fromIterator(it)

  def itemsParser: Parser[Chunk[Long]] =
    case s"Starting items: ${items}" =>
      Chunk.fromArray(items.split(",")).map(_.trim.toLong)

  def operationParser: Parser[Long => Long] =
    case s"Operation: new = old * old"  => i => i * i
    case s"Operation: new = old + $num" => _ + num.toLong
    case s"Operation: new = old * $num" => _ * num.toLong

  def testConditionParser: Parser[Int] =
    case s"Test: divisible by $num" => num.toInt

  def testOutcomeParser: Parser[Int] =
    case s"If true: throw to monkey $num"  => num.toInt
    case s"If false: throw to monkey $num" => num.toInt
