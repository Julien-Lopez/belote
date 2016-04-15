package interface

import java.text.ParseException

import game.{Card, Team}
import game.color._
import player.Player

import scala.io.StdIn._

object ConsoleInterface extends GameInterface {
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
  override def endPlay(t1 : Team, t2 : Team) =
  {
    Console.println(t1 + ": " + t1.points)
    Console.println(t2 + ": " + t2.points)
  }
  override def endRound(t1 : Team, t2 : Team) =
  {
    Console.println(t1 + ": " + t1.score)
    Console.println(t2 + ": " + t2.score)
  }
  override def wins(winners : Team, losers : Team) =
    Console.println(winners + " wins! Final score: " + winners.points + " / " + losers.points)

  override def readBet() =
  {
    val bet = readLine()
    if (bet.equalsIgnoreCase("call"))
      (0, null)
    else if (bet.matches("[0-9]+ [A-Za-z]+")) {
      val split = bet.split(" ")
      split(1).toUpperCase match
      {
        case "DIAMOND" => (split(0).toInt, DIAMOND)
        case "HEART" => (split(0).toInt, HEART)
        case "SPADE" => (split(0).toInt, SPADE)
        case "CLUB" => (split(0).toInt, CLUB)
        case _ =>
          Console.println("Invalid bet.")
          readBet()
      }
    }
    else {
      Console.println("Invalid bet.")
      readBet()
    }
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
