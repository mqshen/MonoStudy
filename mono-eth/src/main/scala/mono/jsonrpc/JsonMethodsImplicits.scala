package mono.jsonrpc

import java.math.BigInteger

import akka.util.ByteString
import mono.domain.Address
import mono.jsonrpc.EthService.BlockParam
import mono.{Hash, UInt256}
import org.json4s.JsonAST.{JInt, JString, JValue}
import mono.jsonrpc.JsonRpcErrors.InvalidParams

import scala.util.Try
import mono.jsonrpc.PersonalService._

trait JsonMethodsImplicits {

  private def decode(s: String): Array[Byte] = {
    val stripped = s.replaceFirst("^0x", "")
    val normalized = if (stripped.length % 2 == 1) "0" + stripped else stripped
    mono.hexDecode(normalized)
  }

  protected def extractBytes(input: String): Either[JsonRpcError, ByteString] =
    Try(ByteString(decode(input))).toEither.left.map(_ => InvalidParams())

  protected def extractBytes(input: JString): Either[JsonRpcError, ByteString] =
    extractBytes(input.s)

  protected def extractBytes(input: String, size: Int): Either[JsonRpcError, ByteString] =
    extractBytes(input).filterOrElse(_.length == size, InvalidParams(s"Invalid value [$input], expected $size bytes"))

  protected def extractAddress(input: String): Either[JsonRpcError, Address] =
    Try(Address(input)).toEither.left.map(_ => InvalidAddress)

  protected def extractAddress(input: JString): Either[JsonRpcError, Address] =
    extractAddress(input.s)


  protected def extractBlockParam(input: JValue): Either[JsonRpcError, BlockParam] = {
    input match {
      case JString("earliest") => Right(BlockParam.Earliest)
      case JString("latest")   => Right(BlockParam.Latest)
      case JString("pending")  => Right(BlockParam.Pending)
      case other =>
        extractQuantity(other).map(BlockParam.WithNumber)
          .left.map(_ => JsonRpcErrors.InvalidParams(s"Invalid default block param: $other"))
    }
  }

  protected def extractQuantity(input: JValue): Either[JsonRpcError, UInt256] =
    input match {
      case JInt(n) =>
        Right(UInt256(n.bigInteger))

      case JString(s) =>
        Try(UInt256(new BigInteger(1, decode(s)))).toEither.left.map(_ => InvalidParams())

      case _ =>
        Left(InvalidParams("could not extract quantity"))
    }

  protected def encodeAsHex(input: Hash): JString =
    JString(s"0x${input.hexString}")

  protected def encodeAsHex(input: ByteString): JString =
    JString(s"0x${mono.toHexString(input)}")

  protected def encodeAsHex(input: BigInteger): JString =
    JString(s"0x${input.toString(16)}")

  protected def encodeAsHex(input: UInt256): JString =
    JString(input.toHexString)

  protected def encodeAsHex(input: Long): JString =
    JString(s"0x${BigInteger.valueOf(input).toString(16)}")
}
