package player

import game.Card

abstract class Player {
  val name: String
  val cards: List[Card]

  def play() : Card
}
