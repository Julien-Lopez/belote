package game

sealed trait Value
{
  val id: Int

  override def toString: String = this.getClass.getSimpleName.replace("$", "")

  def <(other: Value): Boolean = id < other.id

  def trumpLowerThan(other: Value): Boolean = (this, other) match {
    case (JACK, _) => false
    case (NINE, x) => x == JACK
    case (_, JACK) | (_, NINE) => true
    case _ => id < other.id
  }

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

class Card(val value: Value, val color: Color)
{
  override def toString: String = value.toString + " " + color
}