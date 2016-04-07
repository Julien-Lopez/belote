package game

import game.color._
import game.value._
import interface.ConsoleInterface
import player.Player

import scala.util.Random

object Board {
  /**
    * The deck of cards.
    */
  private val deck = List(new Card(SEVEN, DIAMOND), new Card(EIGHT, DIAMOND), new Card(NINE, DIAMOND),
    new Card(TEN, DIAMOND), new Card(JACK, DIAMOND), new Card(QUEEN, DIAMOND), new Card(KING, DIAMOND),
    new Card(ACE, DIAMOND), new Card(SEVEN, HEART), new Card(EIGHT, HEART), new Card(NINE, HEART), new Card(TEN, HEART),
    new Card(JACK, HEART), new Card(QUEEN, HEART), new Card(KING, HEART), new Card(ACE, HEART), new Card(SEVEN, SPADE),
    new Card(EIGHT, SPADE), new Card(NINE, SPADE), new Card(TEN, SPADE), new Card(JACK, SPADE), new Card(QUEEN, SPADE),
    new Card(KING, SPADE), new Card(ACE, SPADE), new Card(SEVEN, CLUB), new Card(EIGHT, CLUB), new Card(NINE, CLUB),
    new Card(TEN, CLUB), new Card(JACK, CLUB), new Card(QUEEN, CLUB), new Card(KING, CLUB), new Card(ACE, CLUB))
  private var cards = deck
  /**
    * The score to reach to win the game.
    */
  val minWinScore = 1000
  val interface = ConsoleInterface

  def game(p1 : Player, p2 : Player, p3 : Player, p4 : Player) =
  {
    var players = List(p4, p1, p2, p3)
    var score1, score2 = 0
    var bets : List[(Int, Color)] = List.empty
    var roundBet : (Int, Color) = null

    while (score1 < minWinScore && score2 < minWinScore)
    {
      players = players.tail ::: players.take(1) // Next player is dealer
      cards = Random.shuffle(deck) // Shuffle every turn for now
      for (player <- players)
      {
        player.newHand(cards.take(8))
        cards = cards.drop(8)
      }
      interface.dealing()

      // Betting phase
      val currPlayer = Iterator.continually(players).flatten
      var counter = -1 // -1 because at the first iteration the four players can call
      while (counter < 3)
      {
        val p = currPlayer.next()
        interface.bets(p)
        roundBet = p.bet(if (bets.nonEmpty) bets.head else null)
        interface.betting(p, roundBet)
        if (roundBet._1 == 0)
          counter += 1
        else if (roundBet._1 < 0)
          interface.betError("Invalid bet: should be positive, was " + roundBet._1)
        else if (roundBet._1 > 160)
          interface.betError("Invalid bet: cannot exceed 160, was " + roundBet._1)
        else if (roundBet._1 % 10 != 0)
          interface.betError("Invalid bet: should be multiple of 10, was " + roundBet._1)
        else if (bets.nonEmpty && roundBet._1 <= bets.head._1)
          interface.betError("Invalid bet: should be greater than previous bet (" + bets.head._1 + "), was "
            + roundBet._1)
        else
        {
          counter = 0
          bets ::= roundBet
        }
      }

      // Playing phase
      if (bets.nonEmpty)
      {
        roundBet = bets.head
        for (move <- 1 to 8)
        {
          for (p <- players)
          {
            interface.plays(p)
            val card = p.play()
            interface.playing(p, card)
          }
        }
        val p = players.head
        if (p == p1 || p == p3) score1 += roundBet._1 else score2 += roundBet._1
      }
    }
    if (score1 > score2)
      interface.wins(p1, p3, score1, score2)
    else
      interface.wins(p2, p4, score1, score2)
  }
}
