package commonstruggles

object Multiset {
  final case class Interval(left: Int, right: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcOccurrences(intervals: List[Interval], n: Int): List[(Int, Int)] = {
    val Interval(smallest, _): Interval = intervals.minBy(_.left)
    val Interval(_, largest): Interval = intervals.maxBy(_.right)
    val size: Int = largest - smallest + 1

    val started: Array[Int] = Array.fill(size)(0)
    val finished: Array[Int] = Array.fill(size)(0)
    val counts: Array[Int] = Array.fill(size)(0)

    for { Interval(a, b) <- intervals } {
      started(a - smallest) += 1
      finished(b - smallest) += 1
    }

    counts(0) = started(0)
    for { ix <- 1 until size } counts(ix) = counts(ix - 1) + started(ix) - finished(ix - 1)

    (for {
      (count, ix) <- counts.zipWithIndex
      if count != 0
    } yield (ix + smallest, count)).toList
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val intervals: List[Interval] = reader.take(n).map{ line =>
      val List(a, b): List[Int] = convertToIntList(line)
      Interval(a, b)
    }.toList
    val result: List[(Int, Int)] = calcOccurrences(intervals, n)
    result.foreach{ case (i, j) => println(s"$i $j") }
  }
}
