package commonstruggles

object CompareSums {
  private def convertToDoubleList(line: String): List[Double] = line.split(" ").map(_.toDouble).toList

  def compare(array1: List[Double], array2: List[Double]): String = {
    val s1: Double = array1.sum
    val s2: Double = array2.sum
    if (math.abs(s1 - s2) < 1e-6) "SUM(A)=SUM(B)"
    else if (s1 < s2) "SUM(A)<SUM(B)"
    else "SUM(A)>SUM(B)"
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val array1: List[Double] = convertToDoubleList(reader.next())
    val array2: List[Double] = convertToDoubleList(reader.next())
    val result: String = compare(array1, array2)
    println(result)
  }
}
