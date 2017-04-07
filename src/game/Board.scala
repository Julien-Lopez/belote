package game

import interface._
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
  val interface = new GraphicalInterface
  interface.visible = true

  var players: List[Player] = List.empty
  var team1, team2: Team = _
  var bets: List[((Int, Color), Player)] = List.empty
  var roundBet: ((Int, Color), Player) = _

  private def validBet(bet: (Int, Color)) =
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

  def cardScore(c: Card, trump: Color): Int = c.value match {
    case SEVEN | EIGHT => 0
    case NINE => if (c.color == trump) 14 else 0
    case TEN => 10
    case JACK => if (c.color == trump) 20 else 2
    case QUEEN => 3
    case KING => 4
    case ACE => 11
  }

  /*
   * - If first card is trump:
   *   - Play higher than winning card if got any
   *   - Else play a trump card if got any
   * - If first card is not trump:
   *   - Always play a card of the same color of the first card if goy any
   *   - Else if opponent got winning card and is a trump:
   *     - Play a higher trump card if got any
   *     - Else play a trump card if got any
   *   - Else if opponent got winning card and is not a trump, play a trump card if got any
   */
  def validMove(c: Card, p: Player, fCardColor: Color, trumpColor: Color, wCard: Card, pIsLosing: Boolean): Boolean =
    if (fCardColor == null) true
    else if (c.color != fCardColor && p.existCard(c => c.color == fCardColor)) false
    else if (fCardColor == trumpColor && c.value < wCard.value && p.existCard(c => c.color == trumpColor && wCard.value < c.value)) false
    else if (c.color == fCardColor) true
    else if (fCardColor != trumpColor && pIsLosing && wCard.color == trumpColor
      && c.color != trumpColor && p.existCard(c => c.color == trumpColor)) false
    else if (fCardColor != trumpColor && pIsLosing && wCard.color == trumpColor // TODO: Bug on <: 9 and Jack
      && c.value < wCard.value && p.existCard(c => c.color == trumpColor && wCard.value < c.value)) false
    else if (fCardColor != trumpColor && pIsLosing && wCard.color != trumpColor
      && c.color != trumpColor && p.existCard(c => c.color == trumpColor)) false
    else true

  def game(p1: Player, p2: Player, p3: Player, p4: Player)
  {
    players = List(p4, p1, p2, p3)
    team1 = new Team(p1, p3)
    team2 = new Team(p2, p4)
    bets = List.empty
    roundBet = null

    while (team1.score < minWinScore && team2.score < minWinScore)
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
        for (_ <- 1 to 8)
        {
          var points = 0
          var fCardColor: Color = null
          var wCard: Card = null
          var wScore = -1
          var wPlayer: Player = null
          for (p <- players) {
            interface.plays(p)
            var card = p.play()
            while (!validMove(card, p, fCardColor, roundBet._1._2, wCard, team1.belongs(wPlayer) ^ team1.belongs(p)))
            {
              interface.moveError(card)
              p.take(card)
              interface.plays(p)
              card = p.play()
            }
            val cScore = cardScore(card, roundBet._1._2)
            interface.playing(p, card)
            points += cScore
            /*
             * Wins if:
             * - No card before
             * - Is trump and card before is not
             * - Is same color and better card score
             */
            if (wCard == null || (wCard.color != roundBet._1._2 && card.color == roundBet._1._2)
              || (wCard.color == card.color && wScore < cScore))
            {
              wCard = card
              wScore = cScore
              wPlayer = p
            }
            if (fCardColor == null) fCardColor = card.color
          }
          val team = if (team1.belongs(wPlayer)) team1 else team2
          team.points += points
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
}
