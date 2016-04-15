package player

import game.{Board, Card}
import game.color._

class AI(name_ : String) extends Player
{
  override val name: String = name_
  override protected var cards : List[Card] = List.empty

  override def bet() =
    if (Board.bets.head == null) (80, DIAMOND) else if (Board.bets.head._1._1 < 100) (Board.bets.head._1._1 + 10, DIAMOND) else null
  override def play() = cards.head
}
