package dynamicprogramming

object EditDistance {
  final case class Penalties(insertion: Int, deletion: Int, substitution: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  def calcEditDistance(s1: Vector[Char], s2: Vector[Char], n: Int, m: Int, penalties: Penalties): Int = {
    val Penalties(ip, dp, sp) = penalties
    val editDistance: Array[Array[Int]] = Array.fill(n + 1, m + 1)(0)
    for { ix <- 1 to n } editDistance(ix)(0) = ix * dp
    for { jy <- 1 to m } editDistance(0)(jy) = jy * ip

    for {
      ix <- 0 until n
      jy <- 0 until m
    } {
      val deletion: Int = editDistance(ix)(jy + 1) + dp
      val insertion: Int = editDistance(ix + 1)(jy) + ip
      val matching: Int = editDistance(ix)(jy) + (if (s1(ix) == s2(jy)) 0 else sp)
      editDistance(ix + 1)(jy + 1) = List(insertion, deletion, matching).min
    }

    editDistance(n)(m)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val List(n, m): List[Int] = convertToIntList(reader.next())
    val s1: Vector[Char] = reader.next().toVector
    val s2: Vector[Char] = reader.next().toVector
    val penalties: Penalties = {
      val List(ip, dp, sp): List[Int] = convertToIntList(reader.next())
      Penalties(ip, dp, sp)
    }
    val distance: Int = calcEditDistance(s1, s2, n, m, penalties)
    println(distance)
  }
}
