package interface

import game.{Card, Team}
import game.Color
import player.Player

trait GameInterface {
  def dealing()
  def bets(p: Player)
  def betting(p: Player, bet: (Int, Color))
  def betError(msg: String)
  def plays(p: Player)
  def playing(p: Player, c: Card)
  def moveError(c: Card)
  def endPlay(t1: Team, t2: Team)
  def endRound(t1: Team, t2: Team)
  def wins(winners : Team, losers : Team)

  // For interactive (human) players
  def readBet() : (Int, Color)
  def readCard(max: Int) : Int
}
