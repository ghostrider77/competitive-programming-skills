package commonstruggles

object BinaryKnapsack {
  import scala.annotation.tailrec

  final case class Item(weight: Int, value: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def mergeSmallestItems(items: List[Item]): List[Item] = {
    @tailrec
    def loop(xs: List[Item], merged: List[Item]): List[Item] = xs match {
      case Nil => merged
      case List(item) => item.copy(weight = 2) :: merged
      case Item(_, v1) :: Item(_, v2) :: xss => loop(xss, Item(2, v1 + v2) :: merged)
    }

    val sortedItems: List[Item] = items.sortBy(_.value)(Ordering[Int].reverse)
    loop(sortedItems, Nil)
  }

  private def findItemWithLargestValue(items: List[Item]): (Int, List[Item]) = {
    val sortedItems: List[Item] = items.sortBy(_.value)(Ordering[Int].reverse)
    (sortedItems.head.value, sortedItems.tail)
  }

  def solveBinaryKnapsack(items: List[Item], capacity: Int): Int = {
    def checkCapacityParity(smallestItems: List[Item], c: Int, v: Int): (List[Item], Int, Int) = {
      if (c % 2 == 0) (mergeSmallestItems(smallestItems), c, v)
      else if (smallestItems.nonEmpty) {
        val (value, remainingItems): (Int, List[Item]) = findItemWithLargestValue(smallestItems)
        (mergeSmallestItems(remainingItems), c - 1, v + value)
      } else (smallestItems, c - 1, v)
    }

    @tailrec
    def loop(currentCapacity: Int, totalValue: Int, currentItems: List[Item]): Int = {
      if (currentCapacity <= 0) totalValue
      else {
        val (smallestItems, rest): (List[Item], List[Item]) = currentItems.partition(_.weight == 1)
        val (mergedItems, updatedCapacity, updatedValue): (List[Item], Int, Int) =
          checkCapacityParity(smallestItems, currentCapacity, totalValue)
        val updatedItems: List[Item] = (rest ::: mergedItems).map{ case Item(w, v)  => Item(w / 2, v) }
        loop(updatedCapacity / 2, updatedValue, updatedItems)
      }
    }

    loop(capacity, 0, items)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(n, capacity): List[Int] = convertToIntList(reader.next())
    val items: List[Item] = reader.take(n).map { line =>
      val List(w, v): List[Int] = convertToIntList(line)
      Item(w, v)
    }.toList
    val result: Int = solveBinaryKnapsack(items, capacity)
    println(result)
  }
}
