package interface

import game.{Card, Color}
import player.Player

trait GameInterface {
  def dealing()
  def betting(p : Player, bet : (Int, Color))
  def playing(p : Player, c : Card)
}
