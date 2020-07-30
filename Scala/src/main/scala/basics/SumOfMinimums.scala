package basics

object SumOfMinimums {
  import scala.annotation.tailrec

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def sumOfMinimums(array: List[Int]): Long = {
    @tailrec
    def loop(xs: List[Int], acc: Long): Long = xs match {
      case Nil => acc
      case _ :: xss =>
        val (_, sum): (Int, Long) = xs.foldLeft((Int.MaxValue, 0L)){
          case ((m, s), elem) =>
            val min: Int = if (elem < m) elem else m
            (min, s + min)
        }
        loop(xss, acc + sum)
    }
    loop(array, 0L)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val array: List[Int] = convertToIntList(reader.next())
    val result: Long = sumOfMinimums(array)
    println(result)
  }
}
