package dynamicprogramming

object MaximalSumSquare {
  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  def maximalSumSquareMatrix(matrix: Vector[Vector[Int]], n: Int, k: Int): Int = {
    val prefixSum: Array[Array[Int]] = Array.fill(n + 1, n + 1)(0)
    for {
      ix <- 1 to n
      jy <- 1 to n
    } {
      val ps: Int = prefixSum(ix - 1)(jy) + prefixSum(ix)(jy - 1) - prefixSum(ix - 1)(jy - 1) + matrix(ix - 1)(jy - 1)
      prefixSum(ix)(jy) = ps
    }

    (0 to n - k).foldLeft(0){
      case (acc, i) =>
        (0 to n - k).foldLeft(acc){
          (inner, j) =>
            math.max(inner, prefixSum(i + k)(j + k) - prefixSum(i + k)(j) - prefixSum(i)(j + k) + prefixSum(i)(j))
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val Vector(n, k): Vector[Int] = convertToIntVector(reader.next())
    val matrix: Vector[Vector[Int]] = reader.take(n).map(convertToIntVector).toVector
    val result: Int = maximalSumSquareMatrix(matrix, n, k)
    println(result)
  }
}
