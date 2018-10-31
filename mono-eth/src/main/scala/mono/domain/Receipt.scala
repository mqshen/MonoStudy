package mono.domain

import akka.util.ByteString
import mono.Hash
import mono.trie.ByteArraySerializable

object Receipt {
  val byteArraySerializable = new ByteArraySerializable[Receipt] {
    import mono.network.p2p.messages.PV63.ReceiptImplicits._

    override def fromBytes(bytes: Array[Byte]): Receipt = bytes.toReceipt
    override def toBytes(input: Receipt): Array[Byte] = input.toBytes
  }

  val Failure = Hash(Array.emptyByteArray)
  val Success = Hash(Array[Byte](1))
}

final case class Receipt(
  postTxState:       Hash,
  cumulativeGasUsed: Long,
  logsBloomFilter:   ByteString,
  logs:              Seq[TxLogEntry]
) {

}
