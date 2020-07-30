package basics

object ExpressionEvaluation {
  import scala.annotation.tailrec

  private val Operations: Map[Char, (Long, Long) => Long] = Map('+' -> ((a, b) => a + b), '-' -> ((a, b) => a - b))

  private def splitString(string: String): (List[Int], List[Char]) = {
    @tailrec
    def loop(xs: List[Char],
             numbers: List[Int],
             ops: List[Char],
             currentNumber: List[Char]): (List[Int], List[Char]) = xs match {
      case Nil => ((currentNumber.reverse.mkString.toInt :: numbers).reverse, ops.reverse)
      case x :: xss =>
        if (x == '+' || x == '-') {
          val n: Int = currentNumber.reverse.mkString.toInt
          loop(xss, n :: numbers, x :: ops, Nil)
        } else loop(xss, numbers, ops, x :: currentNumber)
    }

    loop(string.toList, Nil, Nil, Nil)
  }

  def calculate(numbers: List[Int], operations: List[Char]): Long =
    numbers.tail.zip(operations).foldLeft(numbers.head.toLong){ case (acc, (n, op)) => Operations(op)(acc, n) }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val string: String = reader.next()
    val (numbers, operations): (List[Int], List[Char]) = splitString(string)
    val result: Long = calculate(numbers, operations)
    println(result)
  }
}
