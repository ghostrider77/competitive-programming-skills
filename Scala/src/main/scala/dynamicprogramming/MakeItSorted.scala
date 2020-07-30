package dynamicprogramming

object MakeItSorted {
  private val Limit: Int = 1000

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def makeArraySorted(array: List[Int], n: Int): Int = {
    val table: Array[Array[Int]] = Array.fill(n, Limit)(0)
    val firstElem: Int = array.head
    for { x <- 1 to Limit } table(0)(x - 1) = math.abs(x - firstElem)

    for {
      (elem, ix) <- array.tail.zipWithIndex
    } {
      val m: Int = table(ix)(0)
      (1 to Limit).foldLeft(m){ case (acc, y) =>
        val updatedM: Int = math.min(acc, table(ix)(y - 1))
        table(ix + 1)(y - 1) = updatedM + math.abs(y - elem)
        updatedM
      }
    }

    (1 to Limit).map(x => table(n - 1)(x - 1)).min
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: List[Int] = convertToIntList(reader.next())
    val result: Int = makeArraySorted(array, n)
    println(result)
  }
}
