package hinz

import collection.immutable.Map

import cats._
import cats.syntax.all._
// import cats.std.all._
import cats.implicits._

package object splendor {

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

  sealed trait GameError
  case object TooManyPlayers extends GameError
  case object TooFewPlayers extends GameError
  case object OutOfOrderPlay extends GameError
  case class InvalidTokenSelection(msg: String) extends GameError
  case class OutOfTokens(t: Set[Color]) extends GameError

  sealed trait Color
  case object Green extends Color
  case object Blue extends Color
  case object Brown extends Color
  case object White extends Color
  case object Red extends Color
  case object Gold extends Color

  sealed trait Tier
  case object Tier1 extends Tier
  case object Tier2 extends Tier
  case object Tier3 extends Tier

  type TokenSet = Map[Color, Int]
  type CardSeq = Seq[Card]

  case class Noble(points: Int, cost: TokenSet)
  case class Card(cardType: Tier, points: Int, bonus: Color, cost: TokenSet)

  case class Player(tokens: TokenSet, cards: Seq[Card], unplayedCards: Seq[Card])
  case class Game(
    tokens: TokenSet,
    decks: Map[Tier, CardSeq],
    nobles: Seq[Noble],
    players: Seq[Player])

  sealed trait Action extends Function2[Game, Player, Either[GameError, Game]] {
    def executeStep(g: Game, p: Player): Either[GameError, (Game, Player)]

    def rotatePlayers(g: Game, p: Player) =
      g.copy(players=g.players.tail :+ p)

    def combine(t1: TokenSet, t2: TokenSet): Either[GameError, TokenSet] = {
      val newTokens = Semigroup.combine(t1, t2)
      val negativeTokens = newTokens
        .filter(_._2 < 0)
        .map(_._1)
        .toSet

      if (negativeTokens.size > 0)
        Left(OutOfTokens(negativeTokens))
      else
        Right(newTokens)
    }


    def apply(g: Game, p: Player) =
      if (g.players.headOption == Some(p))
        for (s <- executeStep(g, p).right)
        yield s match {
          case (g, p) => rotatePlayers(g, p)
        }
      else
        Left(OutOfOrderPlay)
  }

  trait TokenAction extends Action {
    def updateTokens(tokenMap: Map[Color, Int], g: Game, p: Player) =
      for (
        newGameTokens <- combine(Group.inverse(tokenMap), g.tokens).right ;
        newPlayerTokens <- combine(tokenMap, p.tokens).right) yield
        (g.copy(tokens=newGameTokens), p.copy(tokens=newPlayerTokens))
  }

  case class SelectThreeTokens(tokens: Set[Color]) extends TokenAction {
    val tokenMap = tokens
      .map((_, 1))
      .toMap

    def executeStep(g: Game, p: Player) =
      if (tokens.size != 3)
        Left(InvalidTokenSelection("Must select 3 different tokens"))
      else
        updateTokens(tokenMap, g, p)

  }

  case class SelectTwoTokens(color: Color) extends TokenAction {
    def executeStep(g: Game, p: Player) =
      if (g.tokens.getOrElse(color, 0) < 4)
        Left(InvalidTokenSelection("Not enought left after double"))
      else
        updateTokens(Map(color -> 2), g, p)
  }

  case class PlayCard(card: Card) extends Action {
    def executeStep(g: Game, p: Player) = ???
  }

  case class SelectFaceUpCard(card: Card) extends Action {
    def executeStep(g: Game, p: Player) = ???
  }

  case class SelectFaceDownCard(tier: Tier) extends Action {
    def executeStep(g: Game, p: Player) = ???
  }

  object Game {
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
    def baseDecks() = cards
      .groupBy(_.cardType)
      .mapValues(t => util.Random.shuffle(t))

    def game(players: Seq[Player]): Either[GameError, Game] =
      players.size match {
        case 2 => Right(Game(twoPlayerTokens, baseDecks(), randomNobles(3), players))
        case 3 => Right(Game(threePlayerTokens, baseDecks(), randomNobles(4), players))
        case 4 => Right(Game(standardTokens, baseDecks(), randomNobles(5), players))
        case i if i < 2 => Left(TooFewPlayers)
        case _ => Left(TooManyPlayers)
      }
  }

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
    Card(Tier1, 1, Red, Map(White -> 4)),
    Card(Tier1, 0, Red, Map(Blue -> 2, Green -> 1)),
    Card(Tier1, 0, Red, Map(White -> 2, Blue -> 1, Green -> 1, Brown -> 1)),
    Card(Tier1, 0, Red, Map(White -> 2, Red -> 2)),
    Card(Tier1, 0, Red, Map(White -> 3)),
    Card(Tier1, 0, Red, Map(White -> 2, Green -> 1, Brown -> 2)),
    Card(Tier1, 0, Red, Map(White -> 1, Red -> 1, Brown -> 3)),
    Card(Tier1, 0, Red, Map(White -> 1, Blue -> 1, Green -> 1, Brown -> 1)),

    Card(Tier1, 1, White, Map(Green -> 4)),
    Card(Tier1, 0, White, Map(Green -> 1, Blue -> 1, Red -> 1, Brown -> 1)),
    Card(Tier1, 0, White, Map(White -> 3, Blue -> 1, Brown -> 1)),
    Card(Tier1, 0, White, Map(Blue -> 2, Brown -> 2)),
    Card(Tier1, 0, White, Map(Blue -> 3)),
    Card(Tier1, 0, White, Map(Red -> 2, Brown -> 1)),
    Card(Tier1, 0, White, Map(Blue -> 2, Green -> 2, Brown -> 1)),
    Card(Tier1, 0, White, Map(Blue -> 1, Green -> 2, Red -> 1, Brown -> 1)),

    Card(Tier1, 1, Blue, Map(Red -> 4)),
    Card(Tier1, 0, Blue, Map(Brown -> 3)),
    Card(Tier1, 0, Blue, Map(Blue -> 1, Green -> 3, Red -> 1)),
    Card(Tier1, 0, Blue, Map(White -> 1, Green -> 1, Red -> 2, Brown -> 1)),
    Card(Tier1, 0, Blue, Map(Green -> 2, Brown -> 2)),
    Card(Tier1, 0, Blue, Map(White -> 1, Green -> 2, Red -> 2)),
    Card(Tier1, 0, Blue, Map(White -> 1, Green -> 1, Red -> 1, Brown -> 1)),
    Card(Tier1, 0, Blue, Map(White -> 1, Brown -> 2)),

    Card(Tier1, 1, Brown, Map(Blue -> 4)),
    Card(Tier1, 0, Brown, Map(Green -> 1, Red -> 3, Brown -> 1)),
    Card(Tier1, 0, Brown, Map(Green -> 2, Red -> 1)),
    Card(Tier1, 0, Brown, Map(Green -> 3)),
    Card(Tier1, 0, Brown, Map(White -> 1, Blue -> 2, Green -> 1, Red -> 1)),
    Card(Tier1, 0, Brown, Map(White -> 2, Blue -> 2, Red -> 1)),
    Card(Tier1, 0, Brown, Map(White -> 2, Green -> 2)),
    Card(Tier1, 0, Brown, Map(White -> 1, Blue -> 1, Green -> 1, Red -> 1)),

    Card(Tier1, 1, Green, Map(Brown -> 4)),
    Card(Tier1, 0, Green, Map(White -> 1, Blue -> 1, Red -> 1, Brown -> 2)),
    Card(Tier1, 0, Green, Map(White -> 1, Blue -> 1, Red -> 1, Brown -> 1)),
    Card(Tier1, 0, Green, Map(Red -> 3)),
    Card(Tier1, 0, Green, Map(White -> 1, Blue -> 3, Green -> 1)),
    Card(Tier1, 0, Green, Map(Blue -> 1, Red -> 2, Brown -> 2)),
    Card(Tier1, 0, Green, Map(White -> 2, Blue -> 1)),
    Card(Tier1, 0, Green, Map(Blue -> 2, Red -> 2)),

    Card(Tier2, 1, White, Map(Green -> 3, Red -> 2, Brown -> 2)),
    Card(Tier2, 1, White, Map(White -> 2, Blue -> 3, Red -> 3)),
    Card(Tier2, 2, White, Map(Red -> 5)),
    Card(Tier2, 2, White, Map(Green -> 1, Red -> 4, Brown -> 2)),
    Card(Tier2, 2, White, Map(Red -> 5, Brown -> 3)),
    Card(Tier2, 3, White, Map(White -> 6)),

    Card(Tier2, 1, Red, Map(Blue -> 3, Red -> 2, Brown -> 3)),
    Card(Tier2, 1, Red, Map(White -> 2, Red -> 2, Brown -> 3)),
    Card(Tier2, 2, Red, Map(White -> 1, Blue -> 4, Green -> 2)),
    Card(Tier2, 2, Red, Map(Brown -> 5)),
    Card(Tier2, 2, Red, Map(White -> 3, Brown -> 5)),
    Card(Tier2, 3, Red, Map(Red -> 6)),

    Card(Tier2, 1, Blue, Map(Blue -> 2, Green -> 3, Brown -> 3)),
    Card(Tier2, 1, Blue, Map(Blue -> 2, Green -> 2, Red -> 3)),
    Card(Tier2, 2, Blue, Map(Blue -> 5)),
    Card(Tier2, 2, Blue, Map(White -> 5, Blue -> 3)),
    Card(Tier2, 2, Blue, Map(White -> 2, Red -> 1, Brown -> 4)),
    Card(Tier2, 3, Blue, Map(Blue -> 6)),

    Card(Tier2, 1, Green, Map(White -> 2, Blue -> 3, Brown -> 2)),
    Card(Tier2, 1, Green, Map(White -> 3, Green -> 2, Red -> 3)),
    Card(Tier2, 2, Green, Map(Blue -> 5, Green -> 3)),
    Card(Tier2, 2, Green, Map(Green -> 5)),
    Card(Tier2, 2, Green, Map(White -> 4, Blue -> 2, Brown -> 1)),
    Card(Tier2, 3, Green, Map(Green -> 6)),

    Card(Tier2, 1, Brown, Map(White -> 3, Blue -> 2, Green -> 2)),
    Card(Tier2, 1, Brown, Map(White -> 3, Green -> 3, Brown -> 2)),
    Card(Tier2, 2, Brown, Map(Green -> 5, Red -> 3)),
    Card(Tier2, 2, Brown, Map(Blue -> 1, Green -> 4, Red -> 2)),
    Card(Tier2, 2, Brown, Map(White -> 5)),
    Card(Tier2, 3, Brown, Map(Brown -> 6)),

    Card(Tier3, 3, Green, Map(White -> 5, Blue -> 3, Red -> 3, Brown -> 3)),
    Card(Tier3, 4, Green, Map(White -> 3, Blue -> 6, Green -> 3)),
    Card(Tier3, 4, Green, Map(Blue -> 7)),
    Card(Tier3, 5, Green, Map(Blue -> 7, Green -> 3)),

    Card(Tier3, 3, Blue, Map(White -> 3, Green -> 3, Red -> 3, Brown -> 5)),
    Card(Tier3, 4, Blue, Map(White -> 6, Blue -> 3, Brown -> 3)),
    Card(Tier3, 4, Blue, Map(White -> 7)),
    Card(Tier3, 5, Blue, Map(White -> 7, Blue -> 3)),

    Card(Tier3, 3, Brown, Map(White -> 3, Blue -> 3, Green -> 5, Red -> 3)),
    Card(Tier3, 4, Brown, Map(Red -> 7)),
    Card(Tier3, 4, Brown, Map(Green -> 3, Red -> 6, Brown -> 3)),
    Card(Tier3, 5, Brown, Map(Red -> 7, Brown -> 3)),

    Card(Tier3, 3, White, Map(Blue -> 3, Green -> 3, Red -> 5, Brown -> 3)),
    Card(Tier3, 4, White, Map(Brown -> 7)),
    Card(Tier3, 4, White, Map(White -> 3, Red -> 3, Brown -> 6)),
    Card(Tier3, 5, White, Map(White -> 3, Brown -> 7)),

    Card(Tier3, 3, Red, Map(White -> 3, Blue -> 5, Green -> 3, Brown -> 3)),
    Card(Tier3, 4, Red, Map(Green -> 7)),
    Card(Tier3, 4, Red, Map(Blue -> 3, Green -> 6, Red -> 3)),
    Card(Tier3, 5, Red, Map(Green -> 7, Red -> 3)))




}