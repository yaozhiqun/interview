import scala.util.Random

sealed trait Suit
case object Spades extends Suit
case object Clubs extends Suit
case object Hearts extends Suit
case object Diamonds extends Suit

case class Card(symbol: String, suit: Suit) {
  override def toString = s"$symbol of $suit"
}

case class Player(num: Int, cards: List[Card]) {
  override def toString = s"Player $num holds ${cards.size} cards including: [${cards.mkString(",")}]"
}

object Deck {
  val suits = Set(Spades, Clubs, Hearts, Diamonds)
  val symbols: Set[String] = (2 to 10).map(_.toString).toSet ++ Set("J", "Q", "K", "A")

  val cards: Set[Card] =
    for {
      suit <- suits
      symbol <- symbols
    } yield Card(symbol, suit)

  def shuffle: List[Card] = Random.shuffle(cards.toList)

  def deal(numPlays: Int): List[Player] = {
    require(Array(2, 4).contains(numPlays), "Only support 2 or 4 players.")

    shuffle.zipWithIndex.foldLeft(Map[Int, List[Card]]()) { case (map, (card, index)) =>
      val n = index % numPlays
      map + (n -> (card :: map.getOrElse(n, Nil)))
    }.toList.map(e => Player(e._1, e._2))
  }

}

object CardGames extends App {
  val players = Deck.deal(4)
  players.foreach(println)

}
