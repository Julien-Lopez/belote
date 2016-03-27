package interface

import game.Card
import game.color.Color
import player.Player

trait GameInterface {
  def dealing()
  def bets(p: Player)
  def betting(p : Player, bet : (Int, Color))
  def plays(p : Player)
  def playing(p : Player, c : Card)

  // For interactive (human) players
  def readBet() : (Int, Color)
  def readCard() : Int
}
