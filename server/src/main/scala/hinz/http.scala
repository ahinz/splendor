package hinz.splendor.http

import hinz.splendor.api._

import de.heikoseeberger.akkahttpcirce._

import io.circe._
import io.circe.generic.auto._

import com.typesafe.config._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

object SplendorService extends App with GameMasterService {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()

  val config = ConfigFactory.load()
  val dao = new InMemoryDao()

  val route = pathPrefix("gm") {
    gmRoute
  } ~
  pathPrefix("ui") {
    encodeResponse {
      getFromDirectory("../client")
    }
  }

  Http().bindAndHandle(route, config.getString("http.interface"), config.getInt("http.port"))
}
