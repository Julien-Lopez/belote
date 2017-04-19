package player

import game._

sealed class DummyAI(override val name: String) extends Player
{
  override def bet(): (Int, Color) = {
    val bet = Array(DIAMOND, HEART, SPADE, CLUB).map(color =>
      ((cards.count(c => c.color == color)
        - (if (cards.exists(c => c.color == color && c.value == JACK)) 0 else 1)
        - (if (cards.exists(c => c.color == color && c.value == NINE)) 0 else 1)
        + cards.count(c => c.color != color && c.value == ACE)) * 20, color)
    ).maxBy(x => x._1)
    if (bet._1 < 80) (0, null)
    else if (Board.bets.isEmpty) bet
    else if (bet._1 <= Board.bets.head._1._1) (0, null)
    else if (bet._2 == Board.bets.head._1._2
      && Board.team1.belongs(this) ^ Board.team1.belongs(Board.bets.head._2)) (0, null)
    else bet
  }

  override def play(): Card = play(cards)

  private def play(cards : List[Card]): Card = if (Board.validMove(cards.head, this)) cards.head else play(cards.tail)
}
