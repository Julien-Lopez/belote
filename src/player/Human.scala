package player

import game.Card

class Human(name_ : String) extends Player {
  override val name: String = name_

  override val cards: List[Card] = List()

  override def play(): Card = ???
}
