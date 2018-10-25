package mono.jsonrpc

import mono.Hash
import mono.jsonrpc.EthService._
import mono.jsonrpc.JsonRpcController.{JsonDecoder, JsonEncoder}
import mono.jsonrpc.JsonRpcErrors.InvalidParams
import org.json4s.JsonAST
import org.json4s.JsonAST.{JArray, JString, JValue}

object EthJsonMethodsImplicits extends JsonMethodsImplicits {

  implicit val eth_sendRawTransaction = new JsonDecoder[SendRawTransactionRequest] with JsonEncoder[SendRawTransactionResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, SendRawTransactionRequest] =
      params match {
        case Some(JArray(JString(dataStr) :: Nil)) =>
          for {
            data <- extractBytes(dataStr)
          } yield SendRawTransactionRequest(data)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: SendRawTransactionResponse): JValue = encodeAsHex(t.transactionHash: Hash)
  }

  implicit val eth_getTransactionCount = new JsonDecoder[GetTransactionCountRequest] with JsonEncoder[GetTransactionCountResponse] {
    def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetTransactionCountRequest] =
      params match {
        case Some(JArray((addressStr: JString) :: (blockValue: JValue) :: Nil)) =>
          for {
            address <- extractAddress(addressStr)
            block <- extractBlockParam(blockValue)
          } yield GetTransactionCountRequest(address, block)
        case _ => Left(InvalidParams())
      }

    def encodeJson(t: GetTransactionCountResponse): JValue = encodeAsHex(t.value)
  }

  implicit val eth_getWork = new JsonDecoder[GetWorkRequest] with JsonEncoder[GetWorkResponse] {
    override def decodeJson(params: Option[JArray]): Either[JsonRpcError, GetWorkRequest] = params match {
      case None | Some(JArray(Nil)) => Right(GetWorkRequest())
      case Some(_)                  => Left(InvalidParams())
    }

    override def encodeJson(t: GetWorkResponse): JsonAST.JValue = {
      val powHeaderHash = encodeAsHex(t.powHeaderHash)
      val dagSeed = encodeAsHex(t.dagSeed)
      val target = encodeAsHex(t.target)
      JArray(List(powHeaderHash, dagSeed, target))
    }
  }
}
