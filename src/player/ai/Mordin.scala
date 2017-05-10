package player.ai

import game._
import player.Player

sealed class Mordin(override val name: String) extends Player
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
    else if (bet._2 == Board.bets.head._1._2 && Board.areNotInSameTeam(this, Board.bets.head._2)) (0, null)
    else bet
  }

  override def play(): Card =
    if (Board.trickWinningCard == null)
      if (Board.areInSameTeam(this, Board.roundBet._2)) playFirstAsBidder(cards) else playFirstAsNonBidder(cards)
    else playNotFirst(cards)

  private def playFirstAsBidder(cards: List[Card]): Card = {
    val trumpPlayed = Board.cardsPlayed.count({case (c, _) => c.color == Board.roundBet._2})
    if (trumpPlayed == 0)
      cards.find(c => c.color == Board.roundBet._2 && c.value == JACK) match {
        case None => cards.minBy(c => c.score(Board.roundBet._1._2))
        case Some(c) => c
      }
    else
      cards.head
  }

  private def playFirstAsNonBidder(cards: List[Card]): Card =
    if (Board.validMove(cards.head, this)) cards.head else playNotFirst(cards.tail)

  private def playNotFirst(cards: List[Card]): Card =
    if (Board.validMove(cards.head, this)) cards.head else playNotFirst(cards.tail)
}
