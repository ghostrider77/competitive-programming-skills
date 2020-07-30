package dynamicprogramming

object SumOfDigits {
  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def solveSumOfDigits(n: Int, length: Int): Long = {
    if (length == 1) if (0 <= n && n <= 9) 1L else 0L
    else {
      val table: Array[Array[Long]] = Array.fill(n + 1, length)(0L)

      for { ix <- 0 to math.min(n, 9) } table(ix)(1) = 1L
      for { jy <- 1 until length } table(0)(jy) = 1L
      for {
        jy <- 2 until length
        ix <- 1 until n
      } table(ix)(jy) = (0 until 10).withFilter(ix >= _).map(k => table(ix - k)(jy - 1)).sum

      (1 until 10).withFilter(n >= _).map(k => table(n - k)(length - 1)).sum
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(n, length): List[Int] = convertToIntList(reader.next())
    val result: Long = solveSumOfDigits(n, length)
    println(result)
  }
}
