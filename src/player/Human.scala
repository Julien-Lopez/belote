package player

import game._

class Human(name_ : String) extends Player {
  override protected val name = name_
  override protected var cards : List[Card] = List.empty

  override def bet(currentBet : (Int, Color)) : (Int, Color) =
  {
    Console.println(name + " bets: ")
    val (qty, color) = scala.io.StdIn.readf2("{0,number,integer} {1}")
    (qty.asInstanceOf[Long].toInt, color.toString match {
    case "DIAMOND" => DIAMOND
    case "HEART" => HEART
    case "SPADE" => SPADE
    case "CLUB" => CLUB
    case _ => null
  })
  }

  override def play() =
  {
    Console.println(cards)
    Console.println("Card to play:")
    cards.head
  }
}
