package commonstruggles

object MostFrequentSymbol {
  import scala.collection.immutable.HashMap

  final case class Query(left: Int, right: Int)

  private def convertToIntList(line: String): List[Int] = line.split(" ").map(_.toInt).toList

  private def findChar(frequencies: HashMap[Char, Int]): Char = frequencies.maxBy{ case (_, count) => count }._1

  private def combine(frequencies1: HashMap[Char, Int], frequencies2: HashMap[Char, Int]): HashMap[Char, Int] =
    frequencies1.merged(frequencies2){ case ((char, count1), (_, count2)) => (char, count1 + count2) }

  private def createSingleCharFrequency(c: Char): HashMap[Char, Int] = HashMap(c -> 1)

  private def calcSegmentTree(chars: Vector[Char], n: Int): Vector[HashMap[Char, Int]] = {
    def log2(x: Double): Double = math.log(x) / math.log(2)
    val maxNrNodes: Int = math.pow(2, math.ceil(log2(n)).toInt + 1).toInt - 1
    val defaultFrequencies: HashMap[Char, Int] = HashMap.empty
    val segmentTree: Array[HashMap[Char, Int]] = Array.fill(maxNrNodes)(defaultFrequencies)

    def buildTree(nodeIndex: Int, leftEnd: Int, rightEnd: Int): Unit = {
      if (leftEnd == rightEnd) segmentTree(nodeIndex) = createSingleCharFrequency(chars(leftEnd))
      else {
        val leftChildIx: Int = 2 * nodeIndex + 1
        val rightChildIx: Int = leftChildIx + 1
        val middleIx: Int = (leftEnd + rightEnd) / 2
        buildTree(leftChildIx, leftEnd, middleIx)
        buildTree(rightChildIx, middleIx + 1, rightEnd)
        segmentTree(nodeIndex) = combine(segmentTree(leftChildIx), segmentTree(rightChildIx))
      }
    }

    buildTree(nodeIndex = 0, leftEnd = 0, rightEnd = n - 1)
    segmentTree.toVector
  }

  def mostFrequentCharsInRange(chars: Vector[Char], queries: List[Query]): List[Char] = {
    val n: Int = chars.length
    val segmentTree: Vector[HashMap[Char, Int]] = calcSegmentTree(chars, n)

    def getFrequenciesInSegment(nodeIx: Int, leftEnd: Int, rightEnd: Int, i: Int, j: Int): HashMap[Char, Int] = {
      if (i <= leftEnd && j >= rightEnd) segmentTree(nodeIx)
      else if (i > rightEnd || j < leftEnd) HashMap.empty
      else {
        val middleIx: Int = (leftEnd + rightEnd) / 2
        val frequencies1: HashMap[Char, Int] = getFrequenciesInSegment(2 * nodeIx + 1, leftEnd, middleIx, i, j)
        val frequencies2: HashMap[Char, Int] = getFrequenciesInSegment(2 * nodeIx + 2, middleIx + 1, rightEnd, i, j)
        combine(frequencies1, frequencies2)
      }
    }

    for { Query(a, b) <- queries } yield {
      val freqs: HashMap[Char, Int] = getFrequenciesInSegment(0, 0, n - 1, a - 1, b - 1)
      findChar(freqs)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val chars: Vector[Char] = reader.next().toVector
    val nrQueries: Int = reader.next().toInt
    val queries: List[Query] = reader.take(nrQueries).map{ line =>
      val List(a, b): List[Int] = convertToIntList(line)
      Query(a, b)
    }.toList
    val result: List[Char] = mostFrequentCharsInRange(chars, queries)
    result.foreach(println)
  }
}
