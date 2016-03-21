package game

import interface.ConsoleInterface
import player.Player

import scala.collection.mutable
import scala.util.Random

object Board {
  /**
    * The deck of cards.
    */
  private var cards = mutable.ListBuffer(new Card(SEVEN, DIAMOND), new Card(EIGHT, DIAMOND), new Card(NINE, DIAMOND),
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
      cards = Random.shuffle(cards) // Shuffle every turn for now
      for (player <- players) player.newHand(cards.take(8).toList) // Deal the cards
      cards.remove(0, 8)
      interface.dealing()

      // Betting phase
      val currPlayer = Iterator.continually(players).flatten
      var counter = 0
      while (counter < 3)
      {
        val p = currPlayer.next()
        roundBet = p.bet(if (bets.nonEmpty) bets.head else null)
        interface.betting(p, roundBet)
        if (roundBet._1 == 0)
          counter += 1
        else
          bets ::= roundBet
      }
      roundBet = bets.head

      // Playing phase
      if (bets.nonEmpty)
      {
        for (move <- 1 to 8)
        {
          for (player <- players)
          {
            cards += player.play()
            interface.playing(player, cards.head)
          }
        }
        val p = players.head
        if (p == p1 || p == p3) score1 += roundBet._1 else score2 += roundBet._1
      }
    }
    if (score1 > score2)
      Console.println(p1 + " and " + p3 + " win!")
    else
      Console.println(p2 + " and " + p4 + " win!")
    Console.println("Final score: " + score1 + " / " + score2)
  }
}
