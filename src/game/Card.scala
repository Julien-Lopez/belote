package game

import game.value._
import game.color._

package object value
{
  sealed trait Value
  {
    val id : Int
    override def toString = this.getClass.getSimpleName.replace("$", "")
    def <(other: Value): Boolean = id < other.id
    def trumpLowerThan(other: Value): Boolean = (this, other) match {
      case (JACK, x) => false
      case (NINE, x) => x == JACK
      case (x, JACK) | (y, NINE) => true
      case _ => id < other.id
    }
  }
  case object SEVEN extends Value { val id = 7 }
  case object EIGHT extends Value { val id = 8 }
  case object NINE extends Value { val id = 9 }
  case object TEN extends Value { val id = 10 }
  case object JACK extends Value { val id = 11 }
  case object QUEEN extends Value { val id = 12 }
  case object KING extends Value { val id = 13 }
  case object ACE extends Value { val id = 14 }
  def toValue(s: String) = s match {
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
}

class Card(v: Value, c: Color)
{
  val value = v
  val color = c
  override def toString = v.toString + " " + c
}