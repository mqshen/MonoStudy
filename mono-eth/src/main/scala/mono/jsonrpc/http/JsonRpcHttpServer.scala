package mono.jsonrpc.http

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.{as, entity, pathEndOrSingleSlash}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import de.heikoseeberger.akkahttpjson4s.Json4sSupport
import mono.jsonrpc.http.JsonRpcHttpServer.JsonRpcHttpServerConfig
import mono.jsonrpc.{JsonRpcController, JsonRpcRequest}
import org.json4s.{DefaultFormats, native}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object JsonRpcHttpServer {
  trait JsonRpcHttpServerConfig {
    val enabled: Boolean
    val interface: String
    val port: Int
  }
}

class JsonRpcHttpServer(jsonRpcController: JsonRpcController, config: JsonRpcHttpServerConfig)(implicit val system: ActorSystem) extends Json4sSupport {
  private val log = Logging(system, this.getClass)

  implicit val serialization = native.Serialization

  implicit val formats = DefaultFormats

  def run() {
    implicit val materializer = ActorMaterializer()

    val corsSettings = CorsSettings.defaultSettings.copy(allowGenericHttpRequests = true)

    val route: Route = cors(corsSettings) {
      (pathEndOrSingleSlash & post) {
        entity(as[JsonRpcRequest]) { request =>
          handleRequest(request)
        } ~ entity(as[Seq[JsonRpcRequest]]) { request =>
          handleBatchRequest(request)
        }
      }
    }

    val bindingResultF = Http(system).bindAndHandle(route, config.interface, config.port)


    bindingResultF onComplete {
      case Success(serverBinding) => log.debug(s"JSON RPC server listening on ${serverBinding.localAddress}")
      case Failure(ex)            => log.error("Cannot start JSON RPC server", ex)
    }
  }

  private def handleRequest(request: JsonRpcRequest) = {
    complete(jsonRpcController.handleRequest(request))
  }

  private def handleBatchRequest(requests: Seq[JsonRpcRequest]) = {
    complete(Future.sequence(requests.map(request => jsonRpcController.handleRequest(request))))
  }

}
