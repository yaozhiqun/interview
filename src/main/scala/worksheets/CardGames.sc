package worksheets

import scala.util.Random

sealed trait Suit
case object Clubs extends Suit
case object Hearts extends Suit
case object Diamonds extends Suit
case object Spades extends Suit

case class Card(symbol: String, suit: Suit)

case class Player(id: Int, cards: Set[Card]) {
  override def toString: String = s"Player $id holds $cards"
}

object Deck extends App {

  val cards = {
    val symbols = (2 to 10).map(_.toString) ++ Set("J", "Q", "K", "A")
    val suits = Set(Clubs, Hearts, Diamonds, Spades)
    for {
      symbol <- symbols
      suit <- suits
    } yield Card(symbol, suit)
  }

  def deal(numOfPlayers: Int): Set[Player] = {
    Random.shuffle(cards).zipWithIndex.foldLeft(Map[Int, Player]()) {
      case (players, (card, index)) =>
        val id = index % numOfPlayers
        val player = players.getOrElse(id, Player(id, Set()))
        players + (id -> player.copy(cards = player.cards + card))
    }.values.toSet
  }

  deal(2).foreach(println)
}


