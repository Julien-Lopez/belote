package game

import game.value._
import game.color._

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