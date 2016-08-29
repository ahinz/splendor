package hinz.splendor.game

import collection.immutable.{Map, Seq, List, Set}

import org.scalatest._

import java.util.UUID

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

trait TestingGame {
  val cards = Seq(
    card(Tier1, 1, Red, Map(White -> 4)),
    card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1)),
    card(Tier1, 0, Red, Map(White -> 2, Blue -> 1, Green -> 1, Brown -> 1)),
    card(Tier1, 0, Red, Map(White -> 2, Red -> 2)),
    card(Tier1, 0, Red, Map(White -> 3)),
    card(Tier1, 0, Red, Map(White -> 2, Green -> 1, Brown -> 2)),
    card(Tier1, 0, Red, Map(White -> 1, Red -> 1, Brown -> 3)),
    card(Tier1, 0, Red, Map(White -> 1, Blue -> 1, Green -> 1, Brown -> 1)))

  val simpleDeck: Map[Tier, CardSeq] = Map(Tier1 -> cards)

  val limitedTokens: TokenSet = Map(Red -> 5, Blue -> 1, Green -> 3, Brown -> 0, Gold -> 2)
  val player = Game.initialPlayer.copy(tokens=Map(Red -> 1))
  val players = Seq(player)


  val game = Game(limitedTokens, simpleDeck, Seq.empty, Seq(player), players.head.id)
}

class TakeTokensSpec extends FlatSpec with Matchers with TestingGame {

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

class TakeCardSpec extends FlatSpec with Matchers with TestingGame {

  "The face up selection" should "be able to selected the top card" in {
    val aCard = cards(1)

    val Right((newGame, newPlayer)) = SelectFaceUpCard(aCard)(game, player)

    player.unplayedCards.contains(aCard) should be(false)
    newPlayer.unplayedCards.contains(aCard) should be(true)

    player.bonusPower should be(Map.empty)
    newPlayer.bonusPower should be(Map.empty)

    newPlayer.tokens.get(Gold) should be(Some(1))
    newGame.tokens.get(Gold) should be(Some(1))
  }

  it should "not be able to buy the card if the card isn't in the top 4" in {
    val aCard = cards(4)

    val Left(c) = SelectFaceUpCard(aCard)(game, player)

    c should be(CardNotInPlay(aCard))
  }

  it should "only give gold if available" in {
    val aCard = cards(1)

    val game2 = game.copy(tokens=Map.empty)
    val Right((newGame, newPlayer)) = SelectFaceUpCard(aCard)(game2, player)

    newPlayer.tokens.get(Gold) should be(None)
  }

  "The face-down selection" should "select a card" in {
    val tgtCard = cards(4)

    val Right((newGame, newPlayer)) = SelectFaceDownCard(Tier1)(game, player)

    player.unplayedCards.contains(tgtCard) should be(false)
    newPlayer.unplayedCards.contains(tgtCard) should be(true)
  }

  "Selecting more than three cards" should "be an error" in {
    val c1 = cards(0)
    val c2 = cards(1)
    val c3 = cards(2)

    val player2 = player.copy(unplayedCards=Seq(c1, c2, c3))
    val game2 = game.copy(players=Seq(player2))

    val Left(e) = SelectFaceDownCard(Tier1)(game2, player2)

    e should be(HiddenCardLimit)
  }
}

class PlayCardSpec extends FlatSpec with Matchers with TestingGame {

  "A player" should "be able to play a card from their hand" in {
    val c1 = card(Tier3, 1, Red, Map(White -> 4))

    val player2 = player.copy(unplayedCards=Seq(c1), tokens=Map(White -> 4))
    val game2 = game.copy(players=Seq(player2))

    val Right((newGame, newPlayer)) = PlayCard(c1)(game2, player2)

    newPlayer.prestigePoints should be(1)
    newPlayer.bonusPower should be(Map(Red -> 1))
    newPlayer.unplayedCards should be(Seq.empty)
  }

  it should "be able to play a visible card from the deck" in {
    val aCard = cards(1)
    val player2 = player.copy(tokens=Map(Blue -> 3, Green -> 2))
    val game2 = game.copy(players=Seq(player2))

    val Right((newGame, newPlayer)) = PlayCard(aCard)(game2, player2)

    newPlayer.prestigePoints should be(0)
    newPlayer.tokens should be(Map(Blue -> 1, Green -> 1, Gold -> 0))
    newPlayer.bonusPower should be(Map(Red -> 1))
    newPlayer.unplayedCards should be(Seq.empty)
  }

  it should "be able to pay with golds if needed" in {
    val aCard = cards(1)
    val player2 = player.copy(tokens=Map(Blue -> 1, Green -> 2, Gold -> 2))
    val game2 = game.copy(players=Seq(player2))

    val Right((newGame, newPlayer)) = PlayCard(aCard)(game2, player2)

    newPlayer.tokens should be(Map(Blue -> 0, Green -> 1, Gold -> 1))
    newPlayer.bonusPower should be(Map(Red -> 1))
    newPlayer.unplayedCards should be(Seq.empty)
  }

  it should "not be able to play if they don't have enough tokens" in {
    val aCard = cards(1)
    val player2 = player.copy(tokens=Map(Blue -> 1, Green -> 2))
    val game2 = game.copy(players=Seq(player2))

    val Left(a) = PlayCard(aCard)(game2, player2)

    a should be(NotEnoughTokensToBuyCard(aCard))
  }
}

//TODO: Test 10 tokens max
class GameSpec extends FlatSpec with Matchers {
    val cards = Seq(
      card(Tier1, 1, Red, Map(White -> 3)),
      card(Tier1, 2, White, Map(Blue -> 2, Green -> 1)),
      card(Tier1, 3, Blue, Map(White -> 2, Blue -> 1, Red -> 1)),
      card(Tier1, 5, Green, Map(White -> 2, Red -> 2)),
      card(Tier1, 8, Red, Map(White -> 3)),
      card(Tier1, 10, Blue, Map(White -> 2, Green -> 1, Brown -> 2)),
      card(Tier1, 13, Red, Map(White -> 1, Red -> 1, Brown -> 3)),
      card(Tier1, 15, Red, Map(White -> 1, Blue -> 1, Green -> 1, Brown -> 1)))

  val simpleDeck: Map[Tier, CardSeq] = Map(Tier1 -> cards)

  val tokens: TokenSet = Map(Red -> 5, Blue -> 5, Green -> 5, Brown -> 5, White -> 5, Gold -> 3)

  val noble1 = Noble(5, Map(White -> 1, Blue -> 1))
  val noble2 = Noble(6, Map(Red -> 1, White -> 1))
  val nobles = Seq(noble1, noble2)

  val player1 = Game.initialPlayer.copy(id=UUID.randomUUID)
  val player2 = Game.initialPlayer.copy(id=UUID.randomUUID)
  val players = Seq(player1, player2)


  val game = Game(tokens, simpleDeck, nobles, players, players.head.id)

  "Players" should "be able to play the game" in {
    // Player 1 selects WBlG
    val Right(GameBeingPlayed(game0)) = game.playTurn(SelectThreeTokens(Set(White, Blue, Green)))
    // Player 2 selects 2W
    val Right(GameBeingPlayed(game1)) = game0.playTurn(SelectTwoTokens(White))
    // Player 1 selects BrGW
    val Right(GameBeingPlayed(game2)) = game1.playTurn(SelectThreeTokens(Set(Brown, Blue, Green)))
    // Player 2 selects RWB
    val Right(GameBeingPlayed(game3)) = game2.playTurn(SelectThreeTokens(Set(Red, Blue, White)))

    val p1Card = cards(1)

    // Player 1 plays a face up card (one red bonus)
    val Right(GameBeingPlayed(game4)) = game3.playTurn(PlayCard(p1Card))

    val p2Card = cards(0)

    // Player 2 gets a red bonus
    val Right(GameBeingPlayed(game5)) = game4.playTurn(PlayCard(p2Card))

    // Player 1 takes a face up card into their hand
    val p1Card2 = cards(5)

    val Right(GameBeingPlayed(game6)) = game5.playTurn(SelectFaceUpCard(p1Card2))

    // Player 2 takes a face down card
    val Right(GameBeingPlayed(game7)) = game6.playTurn(SelectFaceDownCard(Tier1))

    // Player 1 takes WBrG
    val Right(GameBeingPlayed(game8)) = game7.playTurn(SelectThreeTokens(Set(White, Brown, Green)))

    // Player 2 takes a face down card
    val Right(GameBeingPlayed(game9)) = game8.playTurn(SelectFaceDownCard(Tier1))

    // Player 1 plays a face down card
    val Right(GameBeingPlayed(game10)) = game9.playTurn(PlayCard(p1Card2))

    val p2Card2 = cards(2)

    // Player 2 plays a card from the table
    // but player 1 wins the game at the end of the turn
    val Right(GameWon(player, _)) = game10.playTurn(PlayCard(p2Card2))

    player.id should be(player1.id)
  }
}
