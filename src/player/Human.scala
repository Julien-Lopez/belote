package player

import game._

class Human(override val name: String) extends Player
{
  override protected var cards: List[Card] = List.empty

  override def bet(): (Int, Color) = Board.interface.readBet()

  override def play(): Card =
  {
    val card = cards(Board.interface.readCard(cards.length))
    cards = cards.filterNot(c => c == card)
    card
  }
}
