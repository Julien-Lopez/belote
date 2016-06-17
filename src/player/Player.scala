package player

import game.Card
import game.color.Color

trait Player {
  val name : String
  protected var cards : List[Card]

  // TODO: These methods should not be accessible by anyone
  def newHand(cards_ : List[Card]) = cards = cards_
  def take(card : Card) = cards = card :: cards
  def bet() : (Int, Color)
  def play() : Card
  def printCards = cards.mkString(", ")
  def printCardsWithIndexes = printer(cards)
  def existCard(p: Card => Boolean) = cards.exists(p)

  private def printer(cards : List[Card]) : String =
  {
    var index = 0
    def printer_(cards : List[Card]) : String =
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
  override def toString = name
}
