package interface

import java.text.ParseException

import game.Card
import game.color._
import player.Player

import scala.io.StdIn._

object ConsoleInterface extends GameInterface {
  // Board management
  override def dealing() = Console.println("Dealing...")
  override def bets(p: Player) =
  {
    Console println "[" + p + "] " + p.printCards
    Console println "[" + p + "] Your bet:"
  }
  override def betting(p: Player, bet: (Int, Color)) =
    Console.println(p + (if (bet._1 != 0) " bets " + bet._1 + " at " + bet._2 else " calls"))
  override def betError(msg: String) = Console.println(msg)
  override def plays(p: Player)
  {
    Console println "[" + p + "] " + p.printCardsWithIndexes
    Console println "[" + p + "] Card to play:"
  }
  override def playing(p: Player, c: Card) = Console.println(p + " played " + c)
  override def wins(w1: Player, w2: Player, score1: Int, score2: Int) =
    Console.println(w1 + " and " + w2 + " win! Final score: " + score1 + " / " + score2)

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
  override def readCard(max: Int) =
    try
      {
        val res = readf1("{0,number,integer}").asInstanceOf[Long].toInt
        if (res > max || res < 1)
          throw new ParseException("Out of bounds card.", 0)
        res - 1
      }
    catch
    {
      case e: ParseException =>
        Console.println("Invalid card, please enter a number between 1 and " + max)
        readCard(max)
      case e: Throwable => throw e
    }
}
