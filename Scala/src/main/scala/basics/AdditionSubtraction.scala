package basics

object AdditionSubtraction {
  import scala.math.Integral.Implicits._

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def findElemInSequence(y: Int, difference: Int, z: Int): Int = {
    val (q1, r1): (Int, Int) = (z - y) /% difference
    if (r1 == 0 && q1 > 0) 2*q1 - 1
    else {
      val (q2, r2): (Int, Int) = z /% difference
      if (r2 == 0 && q2 > 0) 2*q2 else -1
    }
  }

  def findFirstOccurrenceInSequence(x: Int, y: Int, z: Int): Int = {
    if (z == 0) 0
    else if (z == x) 1
    else {
      val difference: Int = x - y
      if (difference == 0 && z != x) -1 else findElemInSequence(y, difference, z)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(x, y, z): List[Int] = convertToIntList(reader.next())
    val result: Int = findFirstOccurrenceInSequence(x, y, z)
    println(result)
  }
}
