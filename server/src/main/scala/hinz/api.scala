package hinz.splendor.api

import hinz.splendor.game._

import collection.immutable.{Map, Seq, List, Set}

import java.util.UUID

import cats._, cats.data.State, cats.std.all._
import de.heikoseeberger.akkahttpcirce._

import io.circe._
import io.circe.generic.auto._
import io.circe.generic.semiauto._


import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._

case class User(id: UUID)
case class PendingGame(id: UUID, users: Set[User]) {
  def start() =
    for(
      g <- Game.game(users.toList.map(p => Game.player(p.id))).right)
    yield GameWrapper(UUID.randomUUID, g, None)
}

case class ApiAction(
  actionType: String,
  cardId: Option[UUID],
  tokens: Option[TokenSet],
  tier: Option[Int]) {

  def tokenAction(tso: Option[TokenSet]): Either[String, Action] = tso match {
    case Some(ts) => {
      val tsWithoutZeros = ts.filter(_._2 > 0)
      val colors = tsWithoutZeros.keys.toSet
      val doubles = tsWithoutZeros.filter(_._2 == 2)

      if (doubles.size == 1)
        Right(SelectTwoTokens(doubles.keys.head))
      else if (colors.size == 3)
        Right(SelectThreeTokens(colors))
      else
        Left("Invalid token action")
    }
    case None => Left("Invalid token action")
  }

  def intToTier(t: Option[Int]) = t match {
    case Some(1) => Right(SelectFaceDownCard(Tier1))
    case Some(2) => Right(SelectFaceDownCard(Tier2))
    case Some(3) => Right(SelectFaceDownCard(Tier3))
    case Some(t) => Left(s"Invalid tier $t")
    case None => Left("Tier not specified")
  }

  def faceUpCardAction(c: Option[Card]) = c match {
    case Some(c) => Right(SelectFaceUpCard(c))
    case None => Left("Card not found")
  }

  def action(idToCard: Map[UUID, Card]): Either[String, Action] = actionType match {
    case "tokens" => tokenAction(tokens)
    case "face-down-card" => intToTier(tier)
    case "face-up-card" => faceUpCardAction(cardId.flatMap(idToCard.get _))
    case at => Left(s"Invalid action $at")
  }
  // PlayCard,
}


case class GameWrapper(id: UUID, g: Game, winner: Option[UUID]) {
  def view = GameView(id, g.tokens, g.decks.mapValues(_.take(4)), g.nobles, g.players.map(_.id), g.currentPlayerId, winner)
}

case class GameView(
  gameId: UUID, tokens: TokenSet, decks: Map[Tier, CardSeq], nobles: Seq[Noble],
  players: Seq[UUID], currentPlayerId: UUID, winner: Option[UUID])

object JsonImplicits {
  implicit val colorKeyDecoder: KeyDecoder[Color] = new KeyDecoder[Color] {
    def apply(c: String) = c match {
      case "green" => Some(Green)
      case "blue" => Some(Blue)
      case "brown" => Some(Brown)
      case "white" => Some(White)
      case "red" => Some(Red)
      case "gold" => Some(Gold)
      case _ => None
    }
  }

  implicit val actionDecoder: Decoder[ApiAction] = deriveDecoder[ApiAction]

  implicit val colorEncoder: Encoder[Color] = new Encoder[Color] {
    def apply(c: Color) = Json.fromString(c.name)
  }

  implicit val colorKeyEncoder: KeyEncoder[Color] = new KeyEncoder[Color] {
    def apply(c: Color) = c.name
  }

  implicit val tierEncoder: Encoder[Tier] = new Encoder[Tier] {
    def apply(t: Tier) = Json.fromString(t.name)
  }

  implicit val tierKeyEncoder: KeyEncoder[Tier] = new KeyEncoder[Tier] {
    def apply(t: Tier) = t.name
  }

  implicit val gvEncoder: Encoder[GameView] = deriveEncoder[GameView]
  implicit val gvLEncoder: Encoder[List[GameView]] = new ArrayEncoder[List[GameView]] {
    final def encodeArray(a: List[GameView]) = a.map(gvEncoder(_))

  }
  implicit val mapEncoder: Encoder[Map[String, List[GameView]]] = Encoder.encodeMapLike[Map, String, List[GameView]]

}

trait GameDao {
  def users(): Seq[User]
  def pendingGames(): Seq[PendingGame]
  def games(): Seq[GameWrapper]
  def createPendingGame(p: PendingGame): PendingGame
  def createGame(g: GameWrapper): GameWrapper
  def updateGame(g: GameWrapper): GameWrapper
  def createUser(u: User): User

  def updatePendingGame(p: PendingGame): PendingGame
  def startGame(p: PendingGame): Either[GameError, GameWrapper]
}

class InMemoryDao extends GameDao {
  // Probably should store these all in AtomicRefs
  var allUsers = Set(
    User(new UUID(0, 1)),
    User(new UUID(0, 2)))

  var allPendingGames = Map(new UUID(0, 3) -> PendingGame(new UUID(0, 3), allUsers))
  var allGames = Map.empty[UUID, GameWrapper]

  def startGame(p: PendingGame) = {
    allPendingGames -= p.id
    for(game <- p.start().right)
    yield createGame(game)
  }

  def users() = allUsers.toList
  def pendingGames() = allPendingGames.values.toList
  def games() = allGames.values.toList

  def updatePendingGame(p: PendingGame) = {
    allPendingGames += (p.id -> p) ; p
  }

  def updateGame(g: GameWrapper) = {
    allGames += (g.id -> g) ; g
  }

  def createPendingGame(p: PendingGame) =
    updatePendingGame(p)

  def createGame(g: GameWrapper) = updateGame(g)

  def createUser(u: User) = {
    allUsers += u; u
  }
}

trait Service {
  val dao: GameDao

  def parseUUID(s: String): Option[UUID] =
    scala.util.Try { UUID.fromString(s) } toOption


  // I think this can be done with a security directive too
  val withAuth: Directive1[User] = headerValueByName("Authorization").flatMap { auth =>
    val result = for(
      uuid <- parseUUID(auth) ;
      user <- dao.users.filter(_.id == uuid).headOption)
      yield provide(user)

    result.getOrElse(reject(AuthorizationFailedRejection)    )
  }
}

trait GameMasterService extends CirceSupport with Service {
  import JsonImplicits._

  val dao: GameDao

  val gmRoute = path("users") {
    post {
      complete(dao.createUser(User(UUID.randomUUID)))
    }
  } ~
  withAuth { user =>
    pathPrefix("game" / JavaUUID / "play") { gameId =>
      put {
        entity(as[ApiAction]) { apiAction =>

          val gameStatus: Either[String, GameView] = dao.games.filter(_.id == gameId).headOption match {
            case Some(gw@GameWrapper(_, game, _)) => apiAction.action(game.cardsById) match {
              case Left(err) => Left(err)
              case Right(action) => if (game.currentPlayerId == user.id) {
                game.playTurn(action) match {
                  case Left(ge) => Left(ge.toString)
                  case Right(GameBeingPlayed(g)) => Right(dao.updateGame(gw.copy(g=g)).view)
                  case Right(GameWon(p, g)) => Right(dao.updateGame(gw.copy(g=g, winner=Some(p.id))).view)
                }
              } else {
                Left(s"Out of order play (current player is ${game.currentPlayerId})")
              }
            }
            case None => Left("Game not found")
          }

          complete(gameStatus.fold(
            e => HttpResponse(StatusCodes.BadRequest, entity = e.toString),
            s => s))

        }
      }
    } ~
    pathPrefix("active-game") {
      get {
        complete {
          val games = dao.games
            .filter(_.g.players.map(_.id).contains(user.id))
            .map(_.view)
            .toList

          Map("games" -> games)
        }
      }
    } ~
    pathPrefix("pending" / JavaUUID / "start") { id =>
      post {
        complete {
          dao.pendingGames.filter(_.id == id).headOption match {
            case None => HttpResponse(StatusCodes.NotFound)
            case Some(pendingGame) => {
              dao.startGame(pendingGame) match {
                case Right(gw) => gw.view
                case Left(ge) => HttpResponse(StatusCodes.BadRequest, entity = ge.toString)
              }
            }
          }
        }
      } ~
      put {
        complete {
          dao.pendingGames.filter(_.id == id).headOption match {
            case None => HttpResponse(StatusCodes.NotFound)
            case Some(pendingGame) =>
              dao.updatePendingGame(pendingGame.copy(users=pendingGame.users + user))
          }
        }
      }
    } ~
    post {
      complete(dao.createPendingGame(PendingGame(UUID.randomUUID, Set(user))))
    }
  }
}
