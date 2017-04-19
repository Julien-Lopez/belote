package player

import game.Card
import game.Color

import scala.annotation.tailrec

trait Player {
  val name: String
  var cards: List[Card] = List.empty

  def newHand(newCards: List[Card]): Unit = cards = newCards
  def take(card: Card): Unit = cards = card :: cards
  def bet(): (Int, Color)
  def play(): Card
  def printCards: String = cards.mkString(", ")
  def printCardsWithIndexes: String = printer(cards)
  def existCard(p: Card => Boolean): Boolean = cards.exists(p)

  private def printer(cards: List[Card]): String =
  {
    @tailrec
    def printer(index: Int, res: String, cards: List[Card]): String =
      cards match
      {
        case Nil => ""
        case List(c) => res + index + ":" + c
        case c :: rest => printer(index + 1, res + index + ":" + c + ", ", rest)
      }
    printer(1, "", cards)
  }
  override def toString: String = name
}
