package basics

object King {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def maximumNumberOfKings(rows: Int, cols: Int): Long = {
    val emptyCells: Long = math.round(math.ceil(rows / 3.0) * math.ceil(cols / 3.0))
    rows * cols - emptyCells
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(rows, cols): List[Int] = convertToIntList(reader.next())
    val result: Long = maximumNumberOfKings(rows, cols)
    println(result)
  }
}
