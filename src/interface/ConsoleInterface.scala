package interface

import java.text.ParseException

import game.Card
import game.color._
import player.Player

import scala.io.StdIn._

object ConsoleInterface extends GameInterface {
  // Board management
  override def dealing(): Unit = Console.println("Dealing...")
  override def bets(p: Player) =
  {
    Console println "[" + p + "] " + p.printCards
    Console println "[" + p + "] Your bet:"
  }
  override def betting(p: Player, bet: (Int, Color)): Unit =
    Console.println(p + (if (bet._1 != 0) " bets " + bet._1 + " at " + bet._2 else " calls"))
  override def plays(p: Player)
  {
    Console println "[" + p + "] " + p.printCardsWithIndexes
    Console println "[" + p + "] Card to play:"
  }
  override def playing(p: Player, c: Card): Unit = Console.println(p + " played " + c)

  override def readBet() =
    try {
      val (qty, color) = readf2("{0,number,integer} {1}")
      (qty.asInstanceOf[Long].toInt, color.toString match {
        case "DIAMOND" => DIAMOND
        case "HEART" => HEART
        case "SPADE" => SPADE
        case "CLUB" => CLUB
        case _ => null
      })
    }
    catch
    {
      case e: ParseException => (0, null)
      case e: Throwable => throw e
    }
  override def readCard() = readf1("{0,number,integer}").asInstanceOf[Long].toInt - 1
}
