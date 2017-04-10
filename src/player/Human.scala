package player

import game._

class Human(override val name: String) extends Player
{
  override def bet(): (Int, Color) = Board.interface.readBet()

  override def play(): Card = cards(Board.interface.readCard(cards.length))
}
