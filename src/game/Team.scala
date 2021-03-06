package game

import player.Player

sealed class Team private[game](p1: Player, p2: Player)
{
  // The total score of the team
  var score = 0
  // The total points of the team during a round
  var points = 0
  def belongs(p: Player): Boolean = p == p1 || p == p2
  override def toString: String = "Team " + p1 + " and " + p2
}
