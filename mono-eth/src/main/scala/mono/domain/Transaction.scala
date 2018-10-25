package mono.domain

import akka.util.ByteString
import mono.UInt256

final case class Transaction(
  nonce:            UInt256,
  gasPrice:         UInt256,
  gasLimit:         Long,
  receivingAddress: Option[Address],
  value:            UInt256,
  payload:          ByteString ) {

  def isContractCreation: Boolean = receivingAddress.isEmpty

  override def toString: String = {
    s"""Transaction {
       |nonce: $nonce
       |gasPrice: $gasPrice
       |gasLimit: $gasLimit
       |receivingAddress: ${if (receivingAddress.isDefined) mono.toHexString(receivingAddress.get.bytes) else "[Contract creation]"}
       |value: $value wei
       |payload: ${if (isContractCreation) "isContractCreation: " else "TransactionData: "}${mono.toHexString(payload)}
       |}""".stripMargin
  }
}
