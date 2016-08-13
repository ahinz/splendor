package hinz.splendor

import org.scalatest._

import cats._
import cats.std.all._
import cats.syntax.group._

class UtilSpec extends FlatSpec with Matchers {
  "Group instances" should "add" in {
    val v = Map(Red -> 2, Green -> 4) |+| Map(Red -> 9, Brown -> 3)

    v should be (Map(Red -> 11, Brown -> 3, Green -> 4))
  }

  it should "subtract" in {
    val v = Map(Red -> 2, Green -> 4) |-| Map(Red -> 9, Brown -> 3)

    v should be (Map(Red -> -7, Brown -> -3, Green -> 4))
  }
}

class TakeTokensSpec extends FlatSpec with Matchers {

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

class TakeCardSpec extends FlatSpec with Matchers {
  val limitedTokens: TokenSet = Map(Red -> 5, Blue -> 1, Green -> 3, Brown -> 0, Gold -> 2)
  val player = Game.initialPlayer
  val players = Seq(player)

  val cards = Seq(
    Card(Tier1, 1, Red, Map(White -> 4)),
    Card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1)),
    Card(Tier1, 0, Red, Map(White -> 2, Blue -> 1, Green -> 1, Brown -> 1)),
    Card(Tier1, 0, Red, Map(White -> 2, Red -> 2)),
    Card(Tier1, 0, Red, Map(White -> 3)),
    Card(Tier1, 0, Red, Map(White -> 2, Green -> 1, Brown -> 2)),
    Card(Tier1, 0, Red, Map(White -> 1, Red -> 1, Brown -> 3)),
    Card(Tier1, 0, Red, Map(White -> 1, Blue -> 1, Green -> 1, Brown -> 1)))

  val simpleDeck: Map[Tier, CardSeq] = Map(Tier1 -> cards)

  val game = Game(limitedTokens, simpleDeck, Seq.empty, Seq(player))

  "The face up selection" should "be able to selected the top card" in {
    val card = Card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1))

    val Right((newGame, newPlayer)) = SelectFaceUpCard(card)(game, player)

    player.unplayedCards.contains(card) should be(false)
    newPlayer.unplayedCards.contains(card) should be(true)

    player.bonusPower should be(Map.empty)
    newPlayer.bonusPower should be(Map.empty)

    newPlayer.tokens.get(Gold) should be(Some(1))
    newGame.tokens.get(Gold) should be(Some(1))
  }

  it should "not be able to buy the card if the card isn't in the top 4" in {
    val card = Card(Tier1, 0, Red, Map(White -> 3))

    val Left(c) = SelectFaceUpCard(card)(game, player)

    c should be(CardNotInPlay(card))
  }

  it should "only give gold if available" in {
    val card = Card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1))

    val game2 = game.copy(tokens=Map.empty)
    val Right((newGame, newPlayer)) = SelectFaceUpCard(card)(game2, player)

    newPlayer.tokens.get(Gold) should be(None)
  }

  "The face-down selection" should "select a card" in {
    val tgtCard = Card(Tier1, 0, Red, Map(White -> 3))

    val Right((newGame, newPlayer)) = SelectFaceDownCard(Tier1)(game, player)

    player.unplayedCards.contains(tgtCard) should be(false)
    newPlayer.unplayedCards.contains(tgtCard) should be(true)
  }

  "Selecting more than three cards" should "be an error" in {
    val c1 = Card(Tier1, 1, Red, Map(White -> 4))
    val c2 = Card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1))
    val c3 = Card(Tier1, 0, Red, Map(White -> 2, Blue -> 1, Green -> 1, Brown -> 1))

    val player2 = player.copy(unplayedCards=Seq(c1, c2, c3))
    val game2 = game.copy(players=Seq(player2))

    val Left(e) = SelectFaceDownCard(Tier1)(game2, player2)

    e should be(HiddenCardLimit)
  }
}
