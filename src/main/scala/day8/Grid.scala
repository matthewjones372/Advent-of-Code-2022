package day8

import zio.*

final case class Point(x: Int, y: Int)

final class Grid(gridLines: Chunk[String]):
  private val treeGrid = gridLines.map(_.map(_.toInt))
  private val gridSize = treeGrid.size - 1

  def visibleTrees: UIO[Int] =
    Ref.make(Set.empty[Point]).flatMap { pointRef =>
      ZIO.foreach(treeGrid.indices) { i =>
        ZIO.foldLeft(treeGrid.indices)((-1, -1, -1, -1)) { case ((left, right, top, down), j) =>
          for {
            left  <- tallest(j, i, left, pointRef)
            right <- tallest(gridSize - j, i, right, pointRef)
            up    <- tallest(i, j, top, pointRef)
            down  <- tallest(i, gridSize - j, down, pointRef)
          } yield (left, right, up, down)
        }
      } *> pointRef.get.map(_.size)
    }

  def maxView: UIO[Int] =
    Ref.make(0).flatMap { scoreRef =>
      ZIO.foreach(0 to gridSize) { i =>
        ZIO.foreach(0 to gridSize) { j =>
          val currentTree = treeGrid(i)(j)

          val left = ((j - 1) to 0 by -1)
            .dropWhile(treeGrid(i)(_) < currentTree)
            .map(l => j - l)
            .headOption
            .getOrElse(j)

          val right = ((j + 1) to gridSize)
            .dropWhile(treeGrid(i)(_) < currentTree)
            .map(r => r - j)
            .headOption
            .getOrElse(gridSize - j)

          val up = ((i - 1) to 0 by -1)
            .dropWhile(treeGrid(_)(j) < currentTree)
            .map(t => i - t)
            .headOption
            .getOrElse(i)

          val down =
            (i + 1 to gridSize)
              .dropWhile(treeGrid(_)(j) < currentTree)
              .map(b => b - i)
              .headOption
              .getOrElse(gridSize - i)

          scoreRef.getAndUpdate(_.max(left * right * up * down))
        }
      } *> scoreRef.get
    }

  private def tallest(i: Int, j: Int, tallest: Int, pointRef: Ref[Set[Point]]): UIO[Int] =
    val tree   = treeGrid(j)(i)
    val update = if tree > tallest then pointRef.update(_ + Point(i, j)) else ZIO.unit
    update.as(tree.max(tallest))

object Grid {
  def apply(gridLines: Chunk[String]): Grid = new Grid(gridLines)
}
