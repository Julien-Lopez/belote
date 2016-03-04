package game

sealed trait Value
{
  def value : String
  override def toString = value
}

case object SEVEN extends Value { val value = "7" }
case object EIGHT extends Value { val value = "8" }
case object NINE extends Value { val value = "9" }
case object TEN extends Value { val value = "10" }
case object JACK extends Value { val value = "J" }
case object QUEEN extends Value { val value = "Q" }
case object KING extends Value { val value = "K" }
case object ACE extends Value { val value = "A" }

case class UnknownValue(value: String) extends Value

sealed trait Color
{
  def color : String
  override def toString = color
}

case object DIAMOND extends Color { val color = "D" }
case object HEART extends Color { val color = "H" }
case object SPADE extends Color { val color = "S" }
case object CLUB extends Color { val color = "C" }

case class UnknownColor(color: String) extends Color

class Card(value_ : Value, color_ : Color) {
  private val value = value_
  private val color = color_

  def getValue = value
  def getColor = color

  override def toString = value.toString + color
}
