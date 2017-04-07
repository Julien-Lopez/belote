package player

import game.{Board, Card, Color, DIAMOND}

class AI(override val name: String) extends Player
{
  override protected var cards: List[Card] = List.empty

  override def bet(): (Int, Color) =
    if (Board.bets.head == null) (80, DIAMOND) else if (Board.bets.head._1._1 < 100) (Board.bets.head._1._1 + 10, DIAMOND) else null
  override def play(): Card = cards.head
}
