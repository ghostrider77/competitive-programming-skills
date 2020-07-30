package dynamicprogramming

object ChainMatrixMultiplication {
  import scala.collection.mutable.{Map => MutableMap}

  private def convertToLongVector(line: String): Vector[Long] = line.split(" ").map(_.toLong).toVector

  def calcCheapestMatrixMultiplication(sizes: Vector[Long], nrMatrices: Int): Long = {
    val dict: MutableMap[(Int, Int), Long] = MutableMap()

    def solve(i: Int, j: Int): Long = {
      if (j == i + 1) 0L
      else {
        def cost: Long =
          (i + 1 until j).foldLeft(Long.MaxValue) {
            (acc, k) => math.min(acc, solve(i, k) + solve(k, j) + sizes(i) * sizes(j) * sizes(k))
          }
        dict.getOrElseUpdate((i, j), cost)
      }
    }

    solve(0, nrMatrices)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val nrMatrices: Int = reader.next().toInt
    val sizes: Vector[Long] = convertToLongVector(reader.next())
    val result: Long = calcCheapestMatrixMultiplication(sizes, nrMatrices)
    println(result)
  }
}
