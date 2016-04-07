package player

import game._
import game.color._

class Human(name_ : String) extends Player {
  override protected val name = name_
  override protected var cards : List[Card] = List.empty

  override def bet(currentBet : (Int, Color)) = Board.interface.readBet()

  override def play() =
  {
    val card = cards(Board.interface.readCard(cards.length))
    cards = cards.filterNot(c => c == card)
    card
  }
}
