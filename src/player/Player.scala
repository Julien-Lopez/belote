package player

import game.{Color, Card}

trait Player {
  protected val name : String
  protected var cards : List[Card]

  def newHand(cards_ : List[Card]) = cards = cards_
  def bet(currentBet : (Int, Color)) : (Int, Color)
  def play() : Card

  override def toString = name
}
