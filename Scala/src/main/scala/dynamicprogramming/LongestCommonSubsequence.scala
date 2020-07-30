package dynamicprogramming

import scala.annotation.tailrec

object LongestCommonSubsequence {
  private def convertToIntVector(line: String): Vector[Int] = line.split(" ").map(_.toInt).toVector

  private def backtrackLCS(backtrack: Array[Array[Int]], n: Int): (List[Int], List[Int]) = {
    @tailrec
    def loop(ixs1: List[Int], ixs2: List[Int], i: Int, j: Int): (List[Int], List[Int]) = {
      if (i <= 0 || j <= 0) (ixs1, ixs2)
      else if (backtrack(i - 1)(j - 1) == 0) loop((i - 1) :: ixs1, (j - 1) :: ixs2, i - 1, j - 1)
      else if (backtrack(i - 1)(j - 1) == -1) loop(ixs1, ixs2, i - 1, j)
      else loop(ixs1, ixs2, i, j - 1)
    }
    loop(Nil, Nil, n, n)
  }

  def longestCommonSubsequence(s1: Vector[Int], s2: Vector[Int], n: Int): (List[Int], List[Int]) = {
    val longestPath: Array[Array[Int]] = Array.fill(n + 1, n + 1)(0)
    val backtrack: Array[Array[Int]] = Array.fill(n, n)(0)
    for {
      ix <- 0 until n
      jy <- 0 until n
    } {
      val mismatch: Int = if (s1(ix) == s2(jy)) 1 else 0
      val path: Int = List(longestPath(ix)(jy + 1), longestPath(ix + 1)(jy), longestPath(ix)(jy) + mismatch).max
      longestPath(ix + 1)(jy + 1) = path
      if (path == longestPath(ix)(jy + 1)) backtrack(ix)(jy) = -1
      if (path == longestPath(ix + 1)(jy)) backtrack(ix)(jy) = 1
    }

    backtrackLCS(backtrack, n)
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val n: Int = reader.next().toInt
    val s1: Vector[Int] = convertToIntVector(reader.next())
    val s2: Vector[Int] = convertToIntVector(reader.next())
    val (ixs1, ixs2): (List[Int], List[Int]) = longestCommonSubsequence(s1, s2, n)
    println(ixs1.length)
    println(ixs1.mkString(" "))
    println(ixs2.mkString(" "))
  }
}
