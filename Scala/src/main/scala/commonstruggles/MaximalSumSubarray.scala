package commonstruggles

object MaximalSumSubarray {
  import scala.annotation.tailrec

  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def calcPrefixesAndSuffixes(array: Vector[Int], n: Int): (Vector[Long], Vector[Long]) = {
    val prefixSums: Array[Long] = Array.fill(n + 1)(0L)
    val suffixSums: Array[Long] = Array.fill(n + 1)(0L)
    val minPrefixSum: Array[Long] = Array.fill(n + 1)(0L)
    val minSuffixSum: Array[Long] = Array.fill(n + 1)(0L)

    for { ix <- 1 to n } {
      prefixSums(ix) = prefixSums(ix - 1) + array(ix - 1)
      minPrefixSum(ix) = math.min(minPrefixSum(ix - 1), prefixSums(ix))
      suffixSums(n - ix) = suffixSums(n - ix + 1) + array(n - ix)
      minSuffixSum(n - ix) = math.min(minSuffixSum(n - ix + 1), suffixSums(n - ix))
    }

    (minPrefixSum.toVector, minSuffixSum.toVector)
  }

  def calcMaximalSumSubarrays(array: Vector[Int], n: Int): List[Long] = {
    val (minPrefixSum, minSuffixSum): (Vector[Long], Vector[Long]) = calcPrefixesAndSuffixes(array, n)
    val sum: Long = array.foldLeft(0L)(_ + _)

    @tailrec
    def loop(ix: Int, maxSubarraySum: List[Long]): List[Long] =
      if (ix == n) maxSubarraySum.reverse
      else if (ix == 0) loop(ix + 1, (sum - minSuffixSum(1)) :: maxSubarraySum)
      else if (ix == n - 1) loop(ix + 1, (sum - minPrefixSum(n - 1)) :: maxSubarraySum)
      else loop(ix + 1, (sum - (minPrefixSum(ix) + minSuffixSum(ix + 1))) :: maxSubarraySum)

    loop(0, Nil)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val array: Vector[Int] = convertToIntVector(reader.next())
    val result: List[Long] = calcMaximalSumSubarrays(array, n)
    println(result.mkString(" "))
  }
}
