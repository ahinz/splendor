package hinz

import collection.immutable.Map

package object splendor {

  sealed trait GameError
  case object TooManyPlayers extends GameError
  case object TooFewPlayers extends GameError

  sealed trait Color
  case object Green extends Color
  case object Blue extends Color
  case object Brown extends Color
  case object White extends Color
  case object Red extends Color
  case object Gold extends Color

  sealed trait CardType
  case object Tier1 extends CardType
  case object Tier2 extends CardType
  case object Tier3 extends CardType

  type TokenSet = Map[Color, Int]
  type CardSeq = Seq[Card]

  case class Noble(points: Int, cost: TokenSet)
  case class Card(cardType: CardType, points: Int, bonus: Color, cost: TokenSet)

  case class Player(tokens: TokenSet, cards: Seq[Card], unplayedCards: Seq[Card])
  case class Game(
    tokens: TokenSet,
    decks: Map[CardType, CardSeq],
    nobles: Seq[Noble],
    players: Seq[Player])

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
