package game

sealed trait Value
{
  val id: Int

  override def toString: String = this.getClass.getSimpleName.replace("$", "")

  def toValue(s: String): Value = s match {
    case "SEVEN" => SEVEN
    case "EIGHT" => EIGHT
    case "NINE" => NINE
    case "TEN" => TEN
    case "JACK" => JACK
    case "QUEEN" => QUEEN
    case "KING" => KING
    case "ACE" => ACE
  }
}

case object SEVEN extends Value {
  override val id: Int = 7
}

case object EIGHT extends Value {
  override val id: Int = 8
}

case object NINE extends Value {
  override val id: Int = 9
}

case object TEN extends Value {
  override val id: Int = 10
}

case object JACK extends Value {
  override val id: Int = 11
}

case object QUEEN extends Value {
  override val id: Int = 12
}

case object KING extends Value {
  override val id: Int = 13
}

case object ACE extends Value {
  override val id: Int = 14
}

sealed trait Color {
  override def toString: String = this.getClass.getSimpleName.replace("$", "")
}

case object DIAMOND extends Color {}

case object HEART extends Color {}

case object SPADE extends Color {}

case object CLUB extends Color {}

class Card(val value: Value, val color: Color) {
  def <(other: Card, trump: Color): Boolean =
    color != trump && other.color == trump || (color == other.color && score(trump) < other.score(trump))

  def score(trump: Color): Int = value match {
    case SEVEN | EIGHT => 0
    case NINE => if (color == trump) 14 else 0
    case TEN => 10
    case JACK => if (color == trump) 20 else 2
    case QUEEN => 3
    case KING => 4
    case ACE => 11
  }

  override def toString: String = value.toString + " " + color
}