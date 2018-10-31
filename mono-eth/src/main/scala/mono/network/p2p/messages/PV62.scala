package mono.network.p2p.messages

import mono.domain.{BlockHeader, SignedTransaction}
import mono.network.p2p.messages.CommonMessages.SignedTransactions
import mono.rlp
import mono.rlp.{RLPEncodeable, RLPList, RLPSerializable}
import mono.rlp.RLPImplicitConversions._
import mono.rlp.RLPImplicits._

object PV62 {
  object BlockBody {
    import SignedTransactions._
    import BlockHeaderImplicits._

    def toBlockBody(rlpEncodeable: RLPEncodeable): BlockBody = rlpEncodeable match {
      case RLPList(transactions: RLPList, uncles: RLPList) =>
        BlockBody(
          transactions.items map SignedTransactions.toSignedTransaction,
          uncles.items map BlockHeaderImplicits.toBlockHeader
        )
      case _ => throw new RuntimeException("Cannot decode BlockBody")
    }

    implicit final class BlockBodyEnc(msg: BlockBody) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = RLPList(
        RLPList(msg.transactionList.map(_.toRLPEncodable): _*),
        RLPList(msg.uncleNodesList.map(_.toRLPEncodable): _*)
      )
    }

    implicit final class BlockBodyDec(val bytes: Array[Byte]) {
      def toBlockBody: BlockBody = BlockBody.toBlockBody(rlp.rawDecode(bytes))
    }
  }

  final case class BlockBody(transactionList: Seq[SignedTransaction], uncleNodesList: Seq[BlockHeader]) {
    override def toString: String =
      s"""BlockBody{
         |transactionList: $transactionList
         |uncleNodesList: $uncleNodesList
         |}
    """.stripMargin
  }

  object BlockHeaderImplicits {

    def toBlockHeader(rlpEncodeable: RLPEncodeable): BlockHeader =
      rlpEncodeable match {
        //case RLPList(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        //logsBloom, difficulty, number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce) =>
        //BlockHeader(parentHash, ommersHash, beneficiary, stateRoot, transactionsRoot, receiptsRoot,
        //  logsBloom, rlp.toUInt256(difficulty), number, gasLimit, gasUsed, unixTimestamp, extraData, mixHash, nonce)
        case RLPList(parentHash, stateRoot, transactionsRoot, receiptsRoot,
          logsBloom, difficulty, number, gasLimit, gasUsed) =>
          BlockHeader(parentHash, stateRoot, transactionsRoot, receiptsRoot, logsBloom, rlp.toUInt256(difficulty), number, gasLimit, gasUsed)
      }

    implicit final class BlockHeaderEnc(blockHeader: BlockHeader) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import blockHeader._
        RLPList(parentHash, stateRoot, transactionsRoot, receiptsRoot, logsBloom, rlp.toRLPEncodable(difficulty), number, gasLimit, gasUsed)
      }
    }

    implicit final class BlockheaderDec(val bytes: Array[Byte]) {
      def toBlockHeader: BlockHeader = BlockheaderEncodableDec(rlp.rawDecode(bytes)).toBlockHeader
    }

    implicit final class BlockheaderEncodableDec(val rlpEncodeable: RLPEncodeable) {
      def toBlockHeader: BlockHeader = BlockHeaderImplicits.toBlockHeader(rlpEncodeable)
    }
  }

}
