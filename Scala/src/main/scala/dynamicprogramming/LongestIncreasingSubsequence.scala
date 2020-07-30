package dynamicprogramming

object LongestIncreasingSubsequence {
  import scala.collection.mutable.ArrayBuffer

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def putNumberToStacks(stacks: ArrayBuffer[List[Int]], n: Int): Unit = {
    stacks.zipWithIndex.find{ case (stack, _) => n <= stack.head } match {
      case None => stacks += List(n)
      case Some((stack, ix)) => stacks(ix) = n :: stack
    }
  }

  def longestIncreasingSubsequence(array: List[Int]): Int = {
    val stacks: ArrayBuffer[List[Int]] = ArrayBuffer(List(Int.MaxValue))
    array.foreach(x => putNumberToStacks(stacks, x))
    stacks.length
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val _: Int = reader.next().toInt
    val array: List[Int] = convertToIntList(reader.next())
    val result: Int = longestIncreasingSubsequence(array)
    println(result)
  }
}
