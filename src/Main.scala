import game.Board
import player.Human

object Main {
  def main(args: Array[String]): Unit = Board.game(new Human("Jack"), new Human("Bob"), new Human("Steve"),
    new Human("Michael"))
}