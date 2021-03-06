package hinz.splendor

import collection.immutable.{Map, Seq, List, Set}

import java.util.UUID

import cats._
import cats.std.all._
import cats.syntax.group._

package object game {

  implicit def mapGroup[K,V](implicit g: Group[V]): Group[Map[K,V]] =
    new Group[Map[K,V]] {
      def inverse(m: Map[K,V]) =
        m.mapValues(g.inverse)

      def empty = Map.empty[K,V]

      def combine(m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
        (m1.keys ++ m2.keys)
          .toSet
          .foldLeft(Map.empty[K,V]) { (newMap, k) =>
          newMap + (k -> g.combine(m1.getOrElse(k, g.empty), m2.getOrElse(k, g.empty)))
        }
    }

  sealed trait GameState { val g: Game }
  case class GameBeingPlayed(g: Game) extends GameState
  case class GameWon(p: Player, g: Game) extends GameState

  sealed trait GameError
  case object CantPickupGold extends GameError
  case object TooManyPlayers extends GameError
  case object TooFewPlayers extends GameError
  case object HiddenCardLimit extends GameError
  case object OutOfOrderPlay extends GameError
  case object FourTokensRequired extends GameError
  case class InvalidTokenSelection(msg: String) extends GameError
  case class OutOfTokens(t: Set[Color]) extends GameError
  case class NotEnoughTokensToBuyCard(c: Card) extends GameError
  case class CardNotInPlay(c: Card) extends GameError

  sealed trait Color { val name: String }
  case object Green extends Color { val name = "green" }
  case object Blue extends Color { val name = "blue" }
  case object Brown extends Color { val name = "brown" }
  case object White extends Color { val name = "white" }
  case object Red extends Color { val name = "red" }
  case object Gold extends Color { val name = "gold" }

  sealed trait Tier { val name: String }
  case object Tier1 extends Tier { val name = "tier1" }
  case object Tier2 extends Tier { val name = "tier2" }
  case object Tier3 extends Tier { val name = "tier3" }

  type TokenSet = Map[Color, Int]
  type CardSeq = Seq[Card]

  case class Noble(points: Int, cost: TokenSet)
  case class Card(id: UUID, cardType: Tier, points: Int, bonus: Color, cost: TokenSet) {
    val bonusPower = Map(bonus -> 1)
  }

  case class Player(id: UUID, tokens: TokenSet, cards: Seq[Card], unplayedCards: Seq[Card], nobles: Seq[Noble]) {
    val bonusPower = cards
      .map(_.bonusPower)
      .fold(Monoid[TokenSet].empty)(Monoid.combine _)

    def addTokens(t: TokenSet) = copy(tokens=tokens |+| t)

    def hasBonusForNoble(n: Noble) =
      (bonusPower |-| n.cost).filter(_._2 < 0).size == 0

    val prestigePoints = cards.map(_.points).sum + nobles.map(_.points).sum
  }

  case class Game(
    tokens: TokenSet,
    decks: Map[Tier, CardSeq],
    nobles: Seq[Noble],
    players: Seq[Player],
    currentPlayerId: UUID) {

    val cardsInPlay = decks.flatMap(_._2.take(4)).toSeq
    val cardsById = cardsInPlay.groupBy(_.id).mapValues(_.head)

    // Error if current player is not in players
    val currentPlayer = players.filter(_.id == currentPlayerId).head
    val nextPlayerIdx = (players.map(_.id).indexOf(currentPlayerId) + 1) % players.size
    val nextPlayerId = players(nextPlayerIdx).id

    def discard(c: Card) =
      copy(decks=decks.mapValues(_.filterNot(_ == c)))

    val winningPlayer = players
      .filter(_.prestigePoints >= 15)
      .sortBy(_.prestigePoints)
      .reverse
      .headOption

    //TODO: Limit to 10 tokens
    def playTurn(action: Action): Either[GameError, GameState] =
      for (gp <- action(this, currentPlayer).right)
      yield {
        val (newGame, newPlayer) = gp

        val (playerNobles, remainingNobles) = nobles.partition(newPlayer.hasBonusForNoble)

        val newPlayers = players.map { player =>
          if (player.id == newPlayer.id)
            newPlayer.copy(nobles=newPlayer.nobles ++ playerNobles)
          else
            player
        }

        val gameForNextTurn = newGame.copy(currentPlayerId=nextPlayerId, players=newPlayers, nobles=remainingNobles)

        // Check for win condition at start of turn
        if (Some(gameForNextTurn.currentPlayerId) == gameForNextTurn.players.headOption.map(_.id))
          gameForNextTurn.winningPlayer match {
            case Some(p) => GameWon(p, gameForNextTurn)
            case None => GameBeingPlayed(gameForNextTurn)
          }
        else
          GameBeingPlayed(gameForNextTurn)
      }
  }

  sealed trait Action {
    def executeStep(g: Game, p: Player): Either[GameError, (Game, Player)]

    def validateTokens(t: TokenSet): Either[GameError, TokenSet] = {
      val negativeTokens = t
        .filter(_._2 < 0)
        .map(_._1)
        .toSet

      if (negativeTokens.size > 0)
        Left(OutOfTokens(negativeTokens))
      else
        Right(t)
    }

    def apply(g: Game, p: Player): Either[GameError, (Game, Player)] =
      if (p.id == g.currentPlayerId)
        executeStep(g, p)
      else
        Left(OutOfOrderPlay)
  }

  trait TokenAction extends Action {
    def updateTokens(tokenMap: Map[Color, Int], g: Game, p: Player) =
      for (
        newGameTokens <- validateTokens(g.tokens |-| tokenMap).right ;
        newPlayerTokens <- validateTokens(tokenMap |+| p.tokens).right) yield
        (g.copy(tokens=newGameTokens), p.copy(tokens=newPlayerTokens))
  }

  case class SelectThreeTokens(tokens: Set[Color]) extends TokenAction {
    val tokenMap = tokens
      .map((_, 1))
      .toMap

    def executeStep(g: Game, p: Player) =
      if (tokens.contains(Gold))
        Left(CantPickupGold)
      else if (tokens.size > 3)
        Left(InvalidTokenSelection("Must select 3 different tokens"))
      else
        updateTokens(tokenMap, g, p)

  }

  case class SelectTwoTokens(color: Color) extends TokenAction {
    def executeStep(g: Game, p: Player) =
      if (color == Gold)
        Left(CantPickupGold)
      else if (g.tokens.getOrElse(color, 0) < 4)
        Left(FourTokensRequired)
      else
        updateTokens(Map(color -> 2), g, p)
  }

  case class PlayCard(card: Card) extends TokenAction {
    def discardFromBoard(g: Game, c: Card) = g.discard(c)
    def discardFromPlayer(p: Player, c: Card) = p.copy(unplayedCards=p.unplayedCards.filterNot(_ == c))

    def executeStep(g: Game, p: Player) =
      if (g.cardsInPlay.contains(card))
        buyCard(discardFromBoard(g, card), p)
      else if (p.unplayedCards.contains(card))
        buyCard(g, discardFromPlayer(p, card))
      else
        Left(CardNotInPlay(card))

    def buyCard(g: Game, p: Player) = {
      val costAfterBonus = (card.cost |-| p.bonusPower).mapValues(c => Math.max(0, c))
      val gameTokensAfterCost = g.tokens |+| costAfterBonus

      val playerTokensAfterCost = p.tokens |-| costAfterBonus
      val additionalTokens = playerTokensAfterCost.values.filter(_ < 0).sum * -1

      val goldTokens = p.tokens.getOrElse(Gold, 0)

      println(s"CAB: $costAfterBonus GAT: $gameTokensAfterCost PTAC: $playerTokensAfterCost)

      if (goldTokens >= additionalTokens) {
        val goldTokensRequired: TokenSet = Map(Gold -> additionalTokens)
        val playerTokensAfterGolds = playerTokensAfterCost.mapValues(c => Math.max(0, c)) |-| goldTokensRequired
        val gameTokensAfterGolds = gameTokensAfterCost |+| goldTokensRequired

        Right((
          g.copy(tokens=gameTokensAfterGolds),
          p.copy(
            tokens=playerTokensAfterGolds,
            cards=p.cards :+ card)))
      } else
        Left(NotEnoughTokensToBuyCard(card))
    }
  }

  trait CardAction extends Action {
    def takeCard(g: Game, p: Player, card: Option[Card]) = {
      val (gameTokensAfterGold, playerTokensAfterGold) =
        if (g.tokens.getOrElse(Gold, 0) > 0)
          (g.tokens |-| Map(Gold -> 1), p.tokens |+| Map(Gold -> 1))
        else
          (g.tokens, p.tokens)

      Right(
        g.copy(tokens=gameTokensAfterGold),
        p.copy(
          unplayedCards=p.unplayedCards ++ card,
          tokens=playerTokensAfterGold))
    }
  }

  case class SelectFaceUpCard(card: Card) extends CardAction {
    def executeStep(g: Game, p: Player) =
      if (g.cardsInPlay.contains(card))
        takeCard(g.discard(card), p, Some(card))
      else
        Left(CardNotInPlay(card))
  }

  case class SelectFaceDownCard(tier: Tier) extends CardAction {
    def executeStep(g: Game, p: Player) =
      if (p.unplayedCards.size >= 3)
        Left(HiddenCardLimit)
      else
        g.decks
          .getOrElse(tier, Seq.empty)
          .drop(4)
          .headOption match {
          case Some(card) => takeCard(g.discard(card), p, Some(card))
          case None => takeCard(g, p, None)
        }
  }

  object Game {
    def player(id: UUID) = Player(id, Map.empty, Seq.empty, Seq.empty, Seq.empty)

    val initialPlayer = player(UUID.randomUUID)

    val standardTokens: TokenSet = Map(Green -> 7, Blue -> 7, Brown -> 7, White -> 7, Red -> 7, Gold -> 5)

    val threePlayerTokens = standardTokens.map {
      case (Gold, v) => (Gold, v)
      case (k, v) => (k, v - 2)
    }

    val twoPlayerTokens = standardTokens.map {
      case (Gold, v) => (Gold, v)
      case (k, v) => (k, v - 3)
    }

    def randomNobles(n: Int) = util.Random.shuffle(nobles).take(n)
    def baseDecks() = {
      cards
        .groupBy(_.cardType)
        .map(kv => (kv._1, util.Random.shuffle(kv._2)))
    }

    def game(players: Seq[Player]): Either[GameError, Game] =
      players.size match {
        case 2 => Right(Game(twoPlayerTokens, baseDecks(), randomNobles(3), players, players.head.id))
        case 3 => Right(Game(threePlayerTokens, baseDecks(), randomNobles(4), players, players.head.id))
        case 4 => Right(Game(standardTokens, baseDecks(), randomNobles(5), players, players.head.id))
        case i if i < 2 => Left(TooFewPlayers)
        case _ => Left(TooManyPlayers)
      }
  }

  def card(cardType: Tier, points: Int, bonus: Color, cost: TokenSet) =
    Card(UUID.randomUUID, cardType, points, bonus, cost)

  val nobles = Seq(
    Noble(3, Map(Brown -> 4, Red -> 4)),
    Noble(3, Map(Blue -> 4, White -> 4)),
    Noble(3, Map(Green -> 3, Blue -> 3, Red -> 3)),
    Noble(3, Map(Brown -> 3, Red -> 3, White -> 3)),
    Noble(3, Map(Red -> 4, Green -> 4)),
    Noble(3, Map(Green -> 3, Blue -> 3, White -> 3)),
    Noble(3, Map(Brown -> 3, Blue -> 3, White -> 3)),
    Noble(3, Map(Blue -> 4, Green -> 4)),
    Noble(3, Map(Brown -> 4, White -> 4)),
    Noble(3, Map(Brown -> 3, Red -> 3, Green -> 3)))

  val cards = Seq(
    card(Tier1, 1, Red, Map(White -> 4)),
    card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1)),
    card(Tier1, 0, Red, Map(White -> 2, Blue -> 1, Green -> 1, Brown -> 1)),
    card(Tier1, 0, Red, Map(White -> 2, Red -> 2)),
    card(Tier1, 0, Red, Map(White -> 3)),
    card(Tier1, 0, Red, Map(White -> 2, Green -> 1, Brown -> 2)),
    card(Tier1, 0, Red, Map(White -> 1, Red -> 1, Brown -> 3)),
    card(Tier1, 0, Red, Map(White -> 1, Blue -> 1, Green -> 1, Brown -> 1)),

    card(Tier1, 1, White, Map(Green -> 4)),
    card(Tier1, 0, White, Map(Green -> 1, Blue -> 1, Red -> 1, Brown -> 1)),
    card(Tier1, 0, White, Map(White -> 3, Blue -> 1, Brown -> 1)),
    card(Tier1, 0, White, Map(Blue -> 2, Brown -> 2)),
    card(Tier1, 0, White, Map(Blue -> 3)),
    card(Tier1, 0, White, Map(Red -> 2, Brown -> 1)),
    card(Tier1, 0, White, Map(Blue -> 2, Green -> 2, Brown -> 1)),
    card(Tier1, 0, White, Map(Blue -> 1, Green -> 2, Red -> 1, Brown -> 1)),

    card(Tier1, 1, Blue, Map(Red -> 4)),
    card(Tier1, 0, Blue, Map(Brown -> 3)),
    card(Tier1, 0, Blue, Map(Blue -> 1, Green -> 3, Red -> 1)),
    card(Tier1, 0, Blue, Map(White -> 1, Green -> 1, Red -> 2, Brown -> 1)),
    card(Tier1, 0, Blue, Map(Green -> 2, Brown -> 2)),
    card(Tier1, 0, Blue, Map(White -> 1, Green -> 2, Red -> 2)),
    card(Tier1, 0, Blue, Map(White -> 1, Green -> 1, Red -> 1, Brown -> 1)),
    card(Tier1, 0, Blue, Map(White -> 1, Brown -> 2)),

    card(Tier1, 1, Brown, Map(Blue -> 4)),
    card(Tier1, 0, Brown, Map(Green -> 1, Red -> 3, Brown -> 1)),
    card(Tier1, 0, Brown, Map(Green -> 2, Red -> 1)),
    card(Tier1, 0, Brown, Map(Green -> 3)),
    card(Tier1, 0, Brown, Map(White -> 1, Blue -> 2, Green -> 1, Red -> 1)),
    card(Tier1, 0, Brown, Map(White -> 2, Blue -> 2, Red -> 1)),
    card(Tier1, 0, Brown, Map(White -> 2, Green -> 2)),
    card(Tier1, 0, Brown, Map(White -> 1, Blue -> 1, Green -> 1, Red -> 1)),

    card(Tier1, 1, Green, Map(Brown -> 4)),
    card(Tier1, 0, Green, Map(White -> 1, Blue -> 1, Red -> 1, Brown -> 2)),
    card(Tier1, 0, Green, Map(White -> 1, Blue -> 1, Red -> 1, Brown -> 1)),
    card(Tier1, 0, Green, Map(Red -> 3)),
    card(Tier1, 0, Green, Map(White -> 1, Blue -> 3, Green -> 1)),
    card(Tier1, 0, Green, Map(Blue -> 1, Red -> 2, Brown -> 2)),
    card(Tier1, 0, Green, Map(White -> 2, Blue -> 1)),
    card(Tier1, 0, Green, Map(Blue -> 2, Red -> 2)),

    card(Tier2, 1, White, Map(Green -> 3, Red -> 2, Brown -> 2)),
    card(Tier2, 1, White, Map(White -> 2, Blue -> 3, Red -> 3)),
    card(Tier2, 2, White, Map(Red -> 5)),
    card(Tier2, 2, White, Map(Green -> 1, Red -> 4, Brown -> 2)),
    card(Tier2, 2, White, Map(Red -> 5, Brown -> 3)),
    card(Tier2, 3, White, Map(White -> 6)),

    card(Tier2, 1, Red, Map(Blue -> 3, Red -> 2, Brown -> 3)),
    card(Tier2, 1, Red, Map(White -> 2, Red -> 2, Brown -> 3)),
    card(Tier2, 2, Red, Map(White -> 1, Blue -> 4, Green -> 2)),
    card(Tier2, 2, Red, Map(Brown -> 5)),
    card(Tier2, 2, Red, Map(White -> 3, Brown -> 5)),
    card(Tier2, 3, Red, Map(Red -> 6)),

    card(Tier2, 1, Blue, Map(Blue -> 2, Green -> 3, Brown -> 3)),
    card(Tier2, 1, Blue, Map(Blue -> 2, Green -> 2, Red -> 3)),
    card(Tier2, 2, Blue, Map(Blue -> 5)),
    card(Tier2, 2, Blue, Map(White -> 5, Blue -> 3)),
    card(Tier2, 2, Blue, Map(White -> 2, Red -> 1, Brown -> 4)),
    card(Tier2, 3, Blue, Map(Blue -> 6)),

    card(Tier2, 1, Green, Map(White -> 2, Blue -> 3, Brown -> 2)),
    card(Tier2, 1, Green, Map(White -> 3, Green -> 2, Red -> 3)),
    card(Tier2, 2, Green, Map(Blue -> 5, Green -> 3)),
    card(Tier2, 2, Green, Map(Green -> 5)),
    card(Tier2, 2, Green, Map(White -> 4, Blue -> 2, Brown -> 1)),
    card(Tier2, 3, Green, Map(Green -> 6)),

    card(Tier2, 1, Brown, Map(White -> 3, Blue -> 2, Green -> 2)),
    card(Tier2, 1, Brown, Map(White -> 3, Green -> 3, Brown -> 2)),
    card(Tier2, 2, Brown, Map(Green -> 5, Red -> 3)),
    card(Tier2, 2, Brown, Map(Blue -> 1, Green -> 4, Red -> 2)),
    card(Tier2, 2, Brown, Map(White -> 5)),
    card(Tier2, 3, Brown, Map(Brown -> 6)),

    card(Tier3, 3, Green, Map(White -> 5, Blue -> 3, Red -> 3, Brown -> 3)),
    card(Tier3, 4, Green, Map(White -> 3, Blue -> 6, Green -> 3)),
    card(Tier3, 4, Green, Map(Blue -> 7)),
    card(Tier3, 5, Green, Map(Blue -> 7, Green -> 3)),

    card(Tier3, 3, Blue, Map(White -> 3, Green -> 3, Red -> 3, Brown -> 5)),
    card(Tier3, 4, Blue, Map(White -> 6, Blue -> 3, Brown -> 3)),
    card(Tier3, 4, Blue, Map(White -> 7)),
    card(Tier3, 5, Blue, Map(White -> 7, Blue -> 3)),

    card(Tier3, 3, Brown, Map(White -> 3, Blue -> 3, Green -> 5, Red -> 3)),
    card(Tier3, 4, Brown, Map(Red -> 7)),
    card(Tier3, 4, Brown, Map(Green -> 3, Red -> 6, Brown -> 3)),
    card(Tier3, 5, Brown, Map(Red -> 7, Brown -> 3)),

    card(Tier3, 3, White, Map(Blue -> 3, Green -> 3, Red -> 5, Brown -> 3)),
    card(Tier3, 4, White, Map(Brown -> 7)),
    card(Tier3, 4, White, Map(White -> 3, Red -> 3, Brown -> 6)),
    card(Tier3, 5, White, Map(White -> 3, Brown -> 7)),

    card(Tier3, 3, Red, Map(White -> 3, Blue -> 5, Green -> 3, Brown -> 3)),
    card(Tier3, 4, Red, Map(Green -> 7)),
    card(Tier3, 4, Red, Map(Blue -> 3, Green -> 6, Red -> 3)),
    card(Tier3, 5, Red, Map(Green -> 7, Red -> 3)))




}
