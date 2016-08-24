package hinz.splendor.api

import hinz.splendor.game._

import java.util.UUID

import cats._, cats.data.State, cats.std.all._
import de.heikoseeberger.akkahttpcirce._

import io.circe._
import io.circe.generic.auto._

import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._

case class User(id: UUID)
case class PendingGame(id: UUID, users: Set[User]) {
  def start() =
    for(
      g <- Game.game(users.toSeq.map(p => Game.player(p.id))).right)
    yield GameWrapper(UUID.randomUUID, g)
}

case class GameWrapper(id: UUID, g: Game) {
  val view = GameView(id, g.tokens, g.decks.mapValues(_.take(4)), g.nobles, g.players.map(_.id), g.currentPlayerId)
}

case class GameView(
  gameId: UUID, tokens: TokenSet, decks: Map[Tier, CardSeq], nobles: Seq[Noble], players: Seq[UUID], currentPlayerId: UUID)

object JsonImplicits {
  implicit val colorEncoder: Encoder[Color] = new Encoder[Color] {
    def apply(c: Color) = Json.fromString(c.name)
  }

  implicit val tierEncoder: Encoder[Tier] = new Encoder[Tier] {
    def apply(t: Tier) = Json.fromString(t.name)
  }

  implicit def deckEncoder(implicit e: Encoder[Map[String, Seq[Card]]]): Encoder[Map[Tier, CardSeq]] = new Encoder[Map[Tier, CardSeq]] {
    def apply(m: Map[Tier, CardSeq]) = e(m.map { kv => (kv._1.name, kv._2) })
  }

  implicit def tokenEncoder(implicit e: Encoder[Map[String, Int]]): Encoder[TokenSet] = new Encoder[TokenSet] {
    def apply(t: TokenSet) = Json.fromFields(t.map { kv => (kv._1.name, Json.fromInt(kv._2)) })
  }
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

  def users() = allUsers.toSeq
  def pendingGames() = allPendingGames.values.toSeq
  def games() = allGames.values.toSeq

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
    pathPrefix("pending") {
      pathPrefix(JavaUUID) { id =>
        path("start") {
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
      }
    } ~
    post {
      complete(dao.createPendingGame(PendingGame(UUID.randomUUID, Set(user))))
    }
  }
}
