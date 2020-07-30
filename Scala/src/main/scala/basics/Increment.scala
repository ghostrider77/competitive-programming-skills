package basics

object Increment {
  private def incrementLength(digits: String): Int = if (digits.forall(_ == '9')) digits.length + 1 else digits.length

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val digits: String = reader.next()
    val result: Int = incrementLength(digits)
    println(result)
  }
}
