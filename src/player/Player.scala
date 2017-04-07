package player

import game.Card
import game.Color

trait Player {
  val name: String
  protected var cards: List[Card] = List.empty

  // TODO: These methods should not be accessible by anyone
  def newHand(newCards: List[Card]): Unit = cards = newCards
  def take(card: Card): Unit = cards = card :: cards
  def bet(): (Int, Color)
  def play(): Card
  def printCards: String = cards.mkString(", ")
  def printCardsWithIndexes: String = printer(cards)
  def existCard(p: Card => Boolean): Boolean = cards.exists(p)

  private def printer(cards: List[Card]): String =
  {
    var index = 0
    def printer_(cards: List[Card]): String =
    {
      cards match
      {
        case Nil => ""
        case List(c) => (index + 1) + ":" + c
        case c :: rest => index += 1; index + ":" + c + ", " + printer_(rest)
      }
    }
    printer_(cards)
  }
  override def toString: String = name
}
