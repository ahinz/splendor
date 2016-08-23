import de.heikoseeberger.akkahttpcirce._

import io.circe._
import io.circe.generic.auto._

import com.typesafe.config._

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

object SplendorService extends App {
  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()

  val config = ConfigFactory.load()

  val route =
    path("hello") {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
      }
    }

  Http().bindAndHandle(route, config.getString("http.interface"), config.getInt("http.port"))
}
