package basics

object ErasingMaximum {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def findArgmax(array: List[Int], maximum: Int, count: Int): Int = {
    val maxIndices: List[Int] = for {
      (item, ix) <- array.zipWithIndex
      if item == maximum
    } yield ix
    if (count >= 3) maxIndices(2) else maxIndices.head
  }

  def removeMaximum(array: List[Int]): List[Int] = {
    val maximum: Int = array.max
    val count: Int = array.count(_ == maximum)
    val maxIx: Int = findArgmax(array, maximum, count)
    for {
      (item, ix) <- array.zipWithIndex
      if ix != maxIx
    } yield item
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val array: List[Int] = convertToIntList(reader.next())
    val result: List[Int] = removeMaximum(array)
    println(result.mkString(" "))
  }
}
