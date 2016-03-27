package player

import game.Card
import game.color._

class AI(name_ : String) extends Player
{
  override protected val name: String = name_
  override protected var cards : List[Card] = List.empty

  override def bet(currentBet : (Int, Color)) =
    if (currentBet == null) (80, DIAMOND) else if (currentBet._1 < 100) (currentBet._1 + 10, DIAMOND) else null
  override def play() = cards.head
}
