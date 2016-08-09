package hinz.splendor

import org.scalatest._

import cats._
import cats.std.all._
import cats.syntax.group._

class SplendorSpec extends FlatSpec with Matchers {

  // "Splendor game"

  // "A Stack" should "pop values in last-in-first-out order" in {
  //   val stack = new Stack[Int]
  //   stack.push(1)
  //   stack.push(2)
  //   stack.pop() should be (2)
  //   stack.pop() should be (1)
  // }

  // it should "throw NoSuchElementException if an empty stack is popped" in {
  //   val emptyStack = new Stack[Int]
  //   a [NoSuchElementException] should be thrownBy {
  //     emptyStack.pop()
  //   }
  // }


  "Group instances" should "add" in {
    val v = Map(Red -> 2, Green -> 4) |+| Map(Red -> 9, Brown -> 3)

    v should be (Map(Red -> 11, Brown -> 3, Green -> 4))
  }

  it should "subtract" in {
    val v = Map(Red -> 2, Green -> 4) |-| Map(Red -> 9, Brown -> 3)

    v should be (Map(Red -> -7, Brown -> -3, Green -> 4))
  }

  val limitedTokens: TokenSet = Map(Red -> 5, Blue -> 1, Green -> 3, Brown -> 0, Gold -> 2)
  val player = Game.initialPlayer.copy(tokens=Map(Red -> 1))
  val players = Seq(player)

  val game = Game(limitedTokens, Map.empty, Seq.empty, players)

  "Selecting three tokens" should "normally work fine" in {
    val Right((newGame, newPlayer)) = SelectThreeTokens(Set(Red, Blue, Green))(game, player)

    newGame.tokens should be (game.tokens |-| Map(Red -> 1, Blue -> 1, Green -> 1))
    newPlayer.tokens should be (player.tokens |+| Map(Red -> 1, Blue -> 1, Green -> 1))
  }

  it should "fail if there aren't enough tokens" in {
    val Left(err) = SelectThreeTokens(Set(Red, Blue, Brown))(game, player)

    err should be(OutOfTokens(Set(Brown)))
  }

  it should "not allow gold tokens" in {
    val Left(err) = SelectThreeTokens(Set(Red, Blue, Gold))(game, player)
    err should be(CantPickupGold)
  }

  "Selecting two tokens" should "normally work fine" in {
    val Right((newGame, newPlayer)) = SelectTwoTokens(Red)(game, player)

    newGame.tokens should be (game.tokens |-| Map(Red -> 2))
    newPlayer.tokens should be (player.tokens |+| Map(Red -> 2))
  }

  it should "require 4 tokens" in {
    val Left(err) = SelectTwoTokens(Green)(game, player)

    err should be(FourTokensRequired)
  }

  it should "not allow gold tokens" in {
    val Left(err) = SelectTwoTokens(Gold)(game, player)
    err should be(CantPickupGold)
  }

}
