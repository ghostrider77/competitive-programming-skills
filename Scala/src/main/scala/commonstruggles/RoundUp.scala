package commonstruggles

object RoundUp {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(x, y): List[Int] = convertToIntList(reader.next())
    val result: Int = if (x % y == 0) x / y else math.ceil(x / y.toDouble).toInt
    println(result)
  }
}
