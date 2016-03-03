import game.{HEART, JACK, Card}

object Main {
  def main(args: Array[String]) = {
    val card = new Card(JACK, HEART)
    (card.getValue, card.getColor) match {
      case (JACK, HEART) => Console.println("ho!")
      case _ => ()
    }
    Console.println(card)
  }
}