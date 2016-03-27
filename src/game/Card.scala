package game

package object value
{
  sealed trait Value { override def toString = this.getClass.getSimpleName.replace("$", "") }
  case object SEVEN extends Value {}
  case object EIGHT extends Value {}
  case object NINE extends Value {}
  case object TEN extends Value {}
  case object JACK extends Value {}
  case object QUEEN extends Value {}
  case object KING extends Value {}
  case object ACE extends Value {}
  case class UnknownValue() extends Value
  def toValue(s : String) = s match {
    case "SEVEN" => SEVEN case "EIGHT" => EIGHT case "NINE" => NINE case "TEN" => TEN
    case "JACK" => JACK case "QUEEN" => QUEEN case "KING" => KING case "ACE" => ACE }
}

package object color
{
  sealed trait Color { override def toString = this.getClass.getSimpleName.replace("$", "") }
  case object DIAMOND extends Color {}
  case object HEART extends Color {}
  case object SPADE extends Color {}
  case object CLUB extends Color {}
  case class UnknownColor(color: String) extends Color
}

class Card(v : value.Value, c : color.Color) { override def toString = v.toString + " " + c }