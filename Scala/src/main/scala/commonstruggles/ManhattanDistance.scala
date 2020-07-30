package commonstruggles

object ManhattanDistance {
  import scala.annotation.tailrec

  final case class Point(x: Int, y: Int, index: Int)
  final case class Value(value: Int, index: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def maximalManhattanDistance(points: List[Point]): List[(Int, Int)] = {
    @tailrec
    def loop(xs: List[Point],
             acc: List[(Int, Int)],
             maxSum: Value,
             maxDiff: Value,
             minSum: Value,
             minDiff: Value): List[(Int, Int)] = xs match {
      case Nil => acc.reverse
      case Point(x, y, ix) :: xss =>
        val nextMaxSum: Value = if (x + y > maxSum.value) Value(x + y, ix) else maxSum
        val nextMinSum: Value = if (x + y < minSum.value) Value(x + y, ix) else minSum
        val nextMaxDiff: Value = if (x - y > maxDiff.value) Value(x - y, ix) else maxDiff
        val nextMinDiff: Value = if (x - y < minDiff.value) Value(x - y, ix) else minDiff
        if (nextMaxSum.value - nextMinSum.value > nextMaxDiff.value - nextMinDiff.value)
          loop(xss, (nextMaxSum.index, nextMinSum.index) :: acc, nextMaxSum, nextMaxDiff, nextMinSum, nextMinDiff)
        else loop(xss, (nextMaxDiff.index, nextMinDiff.index) :: acc, nextMaxSum, nextMaxDiff, nextMinSum, nextMinDiff)
    }

    val Point(x, y, ix) = points.head
    loop(points.tail, List((ix, ix)), Value(x + y, ix), Value(x - y, ix), Value(x + y, ix), Value(x - y, ix))
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val points: List[Point] = reader.take(n).zipWithIndex.map{ case (line, ix) =>
      val List(x, y): List[Int] = convertToIntList(line)
      Point(x, y, ix + 1)
    }.toList
    val result: List[(Int, Int)] = maximalManhattanDistance(points)
    result.foreach{ case (i, j) => println(s"$i $j") }
  }
}
