package interface

import game.Card
import game.color.Color
import player.Player

trait GameInterface {
  def dealing()
  def bets(p: Player)
  def betting(p: Player, bet: (Int, Color))
  def betError(msg: String)
  def plays(p: Player)
  def playing(p: Player, c: Card)
  def wins(w1: Player, w2: Player, score1: Int, score2: Int)

  // For interactive (human) players
  def readBet() : (Int, Color)
  def readCard(max: Int) : Int
}
