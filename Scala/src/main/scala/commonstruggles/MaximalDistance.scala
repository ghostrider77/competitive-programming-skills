package commonstruggles

object MaximalDistance {
  import scala.annotation.tailrec

  final case class Point(x: Int, index: Int)

  def maxDistances(points: List[Point]): List[(Int, Int)] = {
    @tailrec
    def loop(xs: List[Point], acc: List[(Int, Int)], left: Point, right: Point): List[(Int, Int)] = xs match {
      case Nil => acc.reverse
      case p :: xss =>
        val nextLeft: Point = if (p.x < left.x) p else left
        val nextRight: Point = if (p.x > right.x) p else right
        loop(xss, (nextLeft.index, nextRight.index) :: acc, nextLeft, nextRight)
    }

    val first: Point = points.head
    loop(points.tail, List((first.index, first.index)), first, first)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val xs: List[Point] = reader.take(n).zipWithIndex.map{ case (x, ix) => Point(x.toInt, ix + 1) }.toList
    val result: List[(Int, Int)] = maxDistances(xs)
    result.foreach{ case (i, j) => println(s"$i $j") }
  }
}
