package mono.domain

import akka.util.ByteString
import mono.{Hash, UInt256, crypto, rlp}
import mono.network.p2p.messages.PV62.BlockHeaderImplicits._
import mono.rlp.RLPList

object BlockHeader {

  def getEncodedWithoutNonce(blockHeader: BlockHeader): Array[Byte] = {
    val rlpEncoded = blockHeader.toRLPEncodable match {
      case rlpList: RLPList => RLPList(rlpList.items.dropRight(2): _*)
      case _                => throw new Exception("BlockHeader cannot be encoded without nonce and mixHash")
    }
    rlp.encode(rlpEncoded)
  }

}

final case class BlockHeader(
  parentHash:       Hash,
  stateRoot:        Hash,
  transactionsRoot: Hash,
  receiptsRoot:     Hash,
  logsBloom:        ByteString,
  difficulty:       UInt256,
  number:           Long,
  gasLimit:         Long,
  gasUsed:          Long
) {

  /**
    * calculates blockHash for given block header
    * @return - hash that can be used to get block bodies / receipts
    */
  lazy val hash = Hash(crypto.kec256(this.toBytes))

}
