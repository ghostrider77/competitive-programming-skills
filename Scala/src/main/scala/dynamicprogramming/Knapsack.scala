package dynamicprogramming

object Knapsack {
  import scala.annotation.tailrec

  final case class Item(weight: Int, value: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def findItemIndices(knapsack: Array[Array[Int]], items: List[Item], n: Int, capacity: Int): List[Int] = {
    @tailrec
    def loop(indices: List[Int], currentCapacity: Int, xs: List[(Item, Int)]): List[Int] = xs match {
      case (Item(weight, _), ix) :: xss if currentCapacity > 0 =>
        if (currentCapacity < weight) loop(indices, currentCapacity, xss)
        else {
          val next: Int = if (ix == n - 1) 0 else knapsack(currentCapacity - 1)(ix + 1)
          if (knapsack(currentCapacity - 1)(ix) != next) loop(ix :: indices, currentCapacity - weight, xss)
          else loop(indices, currentCapacity, xss)
        }
      case _ => indices.sorted
    }

    loop(Nil, capacity, items.zipWithIndex)
  }

  def solveKnapsackProblem(items: List[Item], nrItems: Int, capacity: Int): List[Int] = {
    val knapsack: Array[Array[Int]] = Array.fill(capacity, nrItems)(-1)

    def solve(currentCapacity: Int, itemsWithIndex: List[(Item, Int)]): Int = itemsWithIndex match {
      case Nil => 0
      case (Item(weight, value), ix) :: rest =>
        if (currentCapacity == 0) 0
        else if (knapsack(currentCapacity - 1)(ix) != -1) knapsack(currentCapacity - 1)(ix)
        else {
          val optimalCapacity: Int =
            if (currentCapacity < weight) solve(currentCapacity, rest)
            else math.max(solve(currentCapacity - weight, rest) + value, solve(currentCapacity, rest))
          knapsack(currentCapacity - 1)(ix) = optimalCapacity
          optimalCapacity
        }
    }

    val _: Int = solve(capacity, items.zipWithIndex)
    findItemIndices(knapsack, items, nrItems, capacity)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(nrItems, capacity): List[Int] = convertToIntList(reader.next())
    val items: List[Item] = reader.take(nrItems).map{ line =>
      val List(weight, value): List[Int] = convertToIntList(line)
      Item(weight, value)
    }.toList
    val result: List[Int] = solveKnapsackProblem(items, nrItems, capacity)
    println(result.length)
    println(result.map(_ + 1).mkString(" "))
  }
}
