package game

import player.Player

import scala.util.Random

object Board {
  /**
    * The deck of cards.
    */
  private var cards = List(new Card(SEVEN, DIAMOND), new Card(EIGHT, DIAMOND), new Card(NINE, DIAMOND),
    new Card(TEN, DIAMOND), new Card(JACK, DIAMOND), new Card(QUEEN, DIAMOND), new Card(KING, DIAMOND),
    new Card(ACE, DIAMOND), new Card(SEVEN, HEART), new Card(EIGHT, HEART), new Card(NINE, HEART), new Card(TEN, HEART),
    new Card(JACK, HEART), new Card(QUEEN, HEART), new Card(KING, HEART), new Card(ACE, HEART), new Card(SEVEN, SPADE),
    new Card(EIGHT, SPADE), new Card(NINE, SPADE), new Card(TEN, SPADE), new Card(JACK, SPADE), new Card(QUEEN, SPADE),
    new Card(KING, SPADE), new Card(ACE, SPADE), new Card(SEVEN, CLUB), new Card(EIGHT, CLUB), new Card(NINE, CLUB),
    new Card(TEN, CLUB), new Card(JACK, CLUB), new Card(QUEEN, CLUB), new Card(KING, CLUB), new Card(ACE, CLUB))
  /**
    * The score to reach to win the game.
    */
  val minWinScore = 1000

  def game(p1 : Player, p2 : Player, p3 : Player, p4 : Player) =
  {
    var players = Iterator.continually(List(p4, p1, p2, p3)).flatten
    var score1, score2 = 0
    var bets : List[(Int, Color)] = List.empty
    var roundBet : (Int, Color) = null
    var counter = 0

    while (score1 < minWinScore && score2 < minWinScore)
    {
      players.next() // Next player is dealer
      val firstPlayer = players
      Random.shuffle(cards) // Shuffle every turn for now
      for (i <- 1 to 4) players.next().newHand(cards.take(8)) // Deal the cards

      // Betting phase
      while (counter < 3)
      {
        roundBet = players.next().bet(roundBet)
        if (roundBet == null)
          counter += 1
        else
          bets ::= roundBet
      }
      roundBet = bets.head

      players = firstPlayer
      // Playing phase
      if (bets.nonEmpty)
      {
        for (i <- 1 to 8)
        {
          for (j <- 1 to 4)
            cards ::= players.next().play()
        }
        val p = firstPlayer.next()
        if (p == p1 || p == p3)
          score1 += roundBet._1
        else
          score2 += roundBet._1
      }
    }
    if (score1 > score2)
      Console.println(p1 + " and " + p3 + " win!")
    else
      Console.println(p2 + " and " + p4 + " win!")
    Console.println("Final score: " + score1 + " / " + score2)
  }
}
