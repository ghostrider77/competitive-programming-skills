package basics

object CheapestPermutation {
  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def readMatrix(reader: Iterator[String], n: Int): Vector[Vector[Int]] =
    reader.take(n).map(convertToIntVector).toVector

  def findCheapestPermutation(matrix: Vector[Vector[Int]], n: Int): List[Int] = {
    def calcCost(permutation: List[Int]): Int =
      permutation.sliding(2).foldLeft(0){ case (cost, List(i, j)) => cost + matrix(i)(j) }

    val xs: List[Int] = (0 until n).toList
    if (n == 1) xs
    else {
      val (minPerm, _): (List[Int], Int) = xs.permutations.foldLeft((xs, calcCost(xs))) {
        case (acc @ (_, minCost), p) =>
          val cost: Int = calcCost(p)
          if (cost < minCost) (p, cost) else acc
      }
      minPerm
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val matrix: Vector[Vector[Int]] = readMatrix(reader, n)
    val result:List[Int] = findCheapestPermutation(matrix, n)
    println(result.map(_ + 1).mkString(" "))
  }
}
