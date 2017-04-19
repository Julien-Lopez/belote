package game

import interface._
import player.ai.Mordin
import player.{Human, Player}

import scala.util.Random

object Board {
  private var isRunning = false
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
  val interface = ConsoleInterface

  /**
    * The score to reach to win the game.
    */
  val minWinScore = 1000

  var players: List[Player] = List.empty
  var team1, team2: Team = _
  var bets: List[((Int, Color), Player)] = List.empty
  var roundBet: ((Int, Color), Player) = _
  var cardsPlayed: List[(Card, Player)] = List.empty
  var roundPoints: Int = 0
  var trickPlayers: List[Player] = List.empty
  var trickFirstCardColor: Color = _
  var trickWinningCard: Card = _
  var trickWinningScore: Int = -1
  var trickWinningPlayer: Player = _

  def validBet(bet: (Int, Color)): Boolean =
  {
    var res = false
    if (bet._1 < 0)
      interface.betError("Invalid bet: should be positive, was " + bet._1)
    else if (bet._1 > 160)
      interface.betError("Invalid bet: cannot exceed 160, was " + bet._1)
    else if (bet._1 % 10 != 0)
      interface.betError("Invalid bet: should be multiple of 10, was " + roundBet._1)
    else if (bet._1 > 0 && bets.nonEmpty && bet._1 <= bets.head._1._1)
      interface.betError("Invalid bet: should be greater than previous bet (" + bets.head._1 + "), was " + bet._1)
    else
      res = true
    res
  }

  def validMove(c: Card, p: Player): Boolean = {
    val pIsLosing = areNotInSameTeam(trickWinningPlayer, p)
    val trumpColor = roundBet._1._2
    if (trickFirstCardColor == null) true
    else if (c.color != trickFirstCardColor && p.existCard(c => c.color == trickFirstCardColor)) false
    else if (trickFirstCardColor == trumpColor && c < (trickWinningCard, trumpColor) && p.existCard(c => trickWinningCard < (c, trumpColor))) false
    else if (c.color == trickFirstCardColor) true
    else if (trickFirstCardColor != trumpColor && pIsLosing && trickWinningCard.color == trumpColor
      && c.color != trumpColor && p.existCard(c => c.color == trumpColor)) false
    else if (trickFirstCardColor != trumpColor && pIsLosing && trickWinningCard.color == trumpColor
      && c < (trickWinningCard, trumpColor) && p.existCard(c => trickWinningCard < (c, trumpColor))) false
    else if (trickFirstCardColor != trumpColor && pIsLosing && trickWinningCard.color != trumpColor
      && c.color != trumpColor && p.existCard(c => c.color == trumpColor)) false
    else true
  }

  def areNotInSameTeam(p1: Player, p2: Player): Boolean = team1.belongs(p1) ^ team1.belongs(p2)
  def areInSameTeam(p1: Player, p2: Player): Boolean = !areNotInSameTeam(p1, p2)

  private def game(p1: Player, p2: Player, p3: Player, p4: Player)
  {
    players = List(p4, p1, p2, p3)
    team1 = new Team(p1, p3)
    team2 = new Team(p2, p4)

    while (team1.score < minWinScore && team2.score < minWinScore)
    {
      bets = List.empty
      roundBet = null
      players = players.tail ::: players.take(1) // Next player is dealer
      trickPlayers = players
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
        do
        {
          roundBet = (p.bet(), p)
        } while (!validBet(roundBet._1))
        interface.betting(p, roundBet._1)
        if (roundBet._1._1 == 0)
          counter += 1
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
        team1.points = 0
        team2.points = 0
        for (round <- 1 to 8)
        {
          while (round > 1 && trickPlayers.head != trickWinningPlayer)
            trickPlayers = trickPlayers.tail ::: trickPlayers.take(1)
          roundPoints = 0
          trickFirstCardColor = null
          trickWinningCard = null
          trickWinningScore = -1
          trickWinningPlayer = null
          for (p <- trickPlayers) {
            interface.plays(p)
            var card = p.play()
            while (!validMove(card, p))
            {
              interface.moveError(card)
              interface.plays(p)
              card = p.play()
            }
            p.cards = p.cards.filterNot(c => c == card)
            val cScore = card.score(roundBet._1._2)
            interface.playing(p, card)
            roundPoints += cScore
            /*
             * Wins if:
             * - No card before
             * - Is trump and card before is not
             * - Is same color and better card score
             */
            if (trickWinningCard == null || (trickWinningCard.color != roundBet._1._2 && card.color == roundBet._1._2)
              || (trickWinningCard.color == card.color && trickWinningScore < cScore))
            {
              trickWinningCard = card
              trickWinningScore = cScore
              trickWinningPlayer = p
            }
            if (trickFirstCardColor == null) trickFirstCardColor = card.color
          }
          val team = if (team1.belongs(trickWinningPlayer)) team1 else team2
          team.points += roundPoints
          interface.endPlay(team1, team2)
        }
        val (team, otherTeam) = if (team1.belongs(roundBet._2)) (team1, team2) else (team2, team1)
        if (team.points >= roundBet._1._1)
          team.score += roundBet._1._1
        else
          otherTeam.score += 160
        interface.endRound(team1, team2)
      }
    }
    if (team1.score > team2.score)
      interface.wins(team1, team2)
    else
      interface.wins(team2, team1)
  }

  def main(args: Array[String]): Unit = {
    if (!isRunning) {
      isRunning = true
      Board.game(new Human("Julien"), new Mordin("Bob"), new Mordin("Steve"), new Mordin("Michael"))
    }
  }
}
