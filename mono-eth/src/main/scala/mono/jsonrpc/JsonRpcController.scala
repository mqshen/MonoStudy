package mono.jsonrpc

import akka.event.Logging
import mono.jsonrpc.EthService._
import mono.jsonrpc.JsonRpcController.JsonRpcConfig
import org.json4s.JsonAST.{JArray, JValue}
import org.json4s.JsonDSL._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object JsonRpcController {
  trait JsonDecoder[T] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, T]
  }

  trait JsonEncoder[T] {
    def encodeJson(t: T): JValue
  }

  trait JsonRpcConfig {
    def apis: Seq[String]
  }

  object Apis {
    val Eth = "eth"
    val Web3 = "web3"
    val Net = "net"
    val Db = "db"
    val Personal = "personal"
    val Admin = "admin"
    val Debug = "debug"
    val Rpc = "rpc"
  }
}

class JsonRpcController(
  ethService: EthService,
  config:     JsonRpcConfig
) {

  import JsonRpcController._
  import EthJsonMethodsImplicits._
  import JsonRpcErrors._

  val apisHandleFns: Map[String, PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]]] = Map(
    Apis.Eth -> handleEthRequest,
    Apis.Db -> PartialFunction.empty,
    Apis.Admin -> PartialFunction.empty,
    Apis.Debug -> PartialFunction.empty
  )

  private def enabledApis = config.apis :+ Apis.Rpc // RPC enabled by default

  private def handleEthRequest: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
    case req @ JsonRpcRequest(_, "eth_sendRawTransaction", _, _) =>
      handle[SendRawTransactionRequest, SendRawTransactionResponse](ethService.sendRawTransaction, req)
    case req @ JsonRpcRequest(_, "eth_getTransactionCount", _, _) =>
      handle[GetTransactionCountRequest, GetTransactionCountResponse](ethService.getTransactionCount, req)
    case req @ JsonRpcRequest(_, "eth_getWork", _, _) =>
      handle[GetWorkRequest, GetWorkResponse](ethService.getWork, req)
    case req =>
      println(req)
      handle[SendRawTransactionRequest, SendRawTransactionResponse](ethService.sendRawTransaction, req)
  }

  def handleRequest(request: JsonRpcRequest): Future[JsonRpcResponse] = {
    val notFoundFn: PartialFunction[JsonRpcRequest, Future[JsonRpcResponse]] = {
      case _ => Future.successful(errorResponse(request, MethodNotFound))
    }

    val handleFn = enabledApis.foldLeft(notFoundFn)((fn, api) => apisHandleFns.getOrElse(api, PartialFunction.empty) orElse fn)
    handleFn(request)
  }

  private def handle[Req, Res](fn: Req => Future[Either[JsonRpcError, Res]], rpcReq: JsonRpcRequest)(implicit dec: JsonDecoder[Req], enc: JsonEncoder[Res]): Future[JsonRpcResponse] = {
    dec.decodeJson(rpcReq.params) match {
      case Right(req) =>
        fn(req)
          .map {
            case Right(success) => successResponse(rpcReq, success)
            case Left(error)    => errorResponse(rpcReq, error)
          }
          .recover {
            case ex =>
              ex.printStackTrace()
              //log.error("Failed to handle RPC request", ex)
              errorResponse(rpcReq, InternalError)
          }
      case Left(error) =>
        Future.successful(errorResponse(rpcReq, error))
    }
  }

  private def successResponse[T](req: JsonRpcRequest, result: T)(implicit enc: JsonEncoder[T]): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, Some(enc.encodeJson(result)), None, req.id.getOrElse(0))

  private def errorResponse[T](req: JsonRpcRequest, error: JsonRpcError): JsonRpcResponse =
    JsonRpcResponse(req.jsonrpc, None, Some(error), req.id.getOrElse(0))
}
