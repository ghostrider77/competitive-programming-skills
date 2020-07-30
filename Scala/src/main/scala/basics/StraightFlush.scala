package basics

object StraightFlush {
  sealed trait Suit
  case object Spades extends Suit
  case object Hearts extends Suit
  case object Diamonds extends Suit
  case object Clubs extends Suit

  object Suit {
    def apply(c: Char): Suit = c match {
      case 'S' => Spades
      case 'H' => Hearts
      case 'D' => Diamonds
      case 'C' => Clubs
      case _ => throw new Exception(s"Unknown suite: $c")
    }
  }

  private val rankToInt: Map[Char, Int] = Map('A' -> 14, 'K' -> 13, 'Q' -> 12, 'J' -> 11, 'T' -> 10)

  final case class Card(suit: Suit, rank: Int)

  object Card {
    def apply(suit: Char, rank: Char): Card = {
      val s: Suit = Suit(suit)
      val r: Int = rankToInt.get(rank) match {
        case Some(n) => n
        case None => rank.asDigit
      }
      Card(s, r)
    }
  }

  private def readCards(line: String): List[Card] = {
    val cards: List[List[Char]] = line.split(" ").map(_.toList).toList
    for { List(r, s) <- cards } yield Card(s, r)
  }

  def isStraightFlush(cards: List[Card]): Boolean = {
    val suits: Set[Suit] = cards.map(_.suit).toSet
    if (suits.size != 1) false
    else {
      val ranks: List[Int] = cards.map(_.rank).sorted
      val differences: List[Int] = ranks.sliding(2).map{ case List(a, b) => b - a }.toList
      differences == List(1, 1, 1, 1) || differences == List(1, 1, 1, 9)
    }
  }

  def main(args: Array[String]): Unit = {
    val reader: Iterator[String] = scala.io.Source.stdin.getLines()
    val cards: List[Card] = readCards(reader.next())
    val result: Boolean = isStraightFlush(cards)
    println(if (result) "YES" else "NO")
  }
}
