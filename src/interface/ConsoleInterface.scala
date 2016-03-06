package interface

import game.{Card, Color}
import player.Player

object ConsoleInterface extends GameInterface {
  override def dealing(): Unit = Console.println("Dealing...")
  override def betting(p: Player, bet: (Int, Color)): Unit =
    Console.println(p + (if (bet != null) " bets " + bet._1 + " at " + bet._2 else " calls"))
  override def playing(p: Player, c: Card): Unit = Console.println(p + " played " + c)
}
