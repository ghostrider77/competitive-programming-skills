package commonstruggles

object YetAnotherSum {
  private def convertToLongList(line: String): List[Long] = line.split(" ").map(_.toLong).toList

  def calculateSum(array: List[Long]): Double = {
    val reciprocals: List[Double] = array.map(1.0 / _)
    val positives: List[Double] = reciprocals.filter(_ > 0).sortBy(math.abs)
    val negatives: List[Double] = reciprocals.filter(_ < 0).sortBy(math.abs)
    (positives.sum + negatives.sum) + array.sum
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val array: List[Long] = convertToLongList(reader.next())
    val result: Double = calculateSum(array)
    println(result)
  }
}
