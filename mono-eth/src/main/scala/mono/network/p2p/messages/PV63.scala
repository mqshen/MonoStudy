package mono.network.p2p.messages

import akka.util.ByteString
import mono.domain.{Account, Address, Receipt, TxLogEntry}
import mono.rlp
import mono.rlp.{RLPEncodeable, RLPList, RLPSerializable}
import mono.rlp.RLPImplicitConversions._
import mono.rlp.RLPImplicits._

object PV63 {

  object AccountImplicits {
    implicit final class AccountEnc(val account: Account) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import account._
        RLPList(rlp.toRLPEncodable(nonce), rlp.toRLPEncodable(balance), stateRoot, codeHash)
      }
    }

    implicit final class AccountDec(val bytes: Array[Byte]) {
      def toAccount: Account = rlp.rawDecode(bytes) match {
        case RLPList(nonce, balance, stateRoot, codeHash) =>
          Account(rlp.toUInt256(nonce), rlp.toUInt256(balance), stateRoot, codeHash)
        case _ => throw new RuntimeException("Cannot decode Account")
      }
    }
  }

  object TxLogEntryImplicits {

    implicit final class TxLogEntryEnc(logEntry: TxLogEntry) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import logEntry._
        RLPList(loggerAddress.bytes, logTopics, data)
      }
    }

    implicit final class TxLogEntryDec(rlpEncodeable: RLPEncodeable) {
      def toTxLogEntry: TxLogEntry = rlpEncodeable match {
        case RLPList(loggerAddress, logTopics: RLPList, data) =>
          TxLogEntry(Address(loggerAddress: ByteString), fromRlpList[ByteString](logTopics), data)

        case _ => throw new RuntimeException("Cannot decode TransactionLog")
      }
    }
  }


  object ReceiptImplicits {
    import TxLogEntryImplicits._

    implicit final class ReceiptEnc(msg: Receipt) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import msg._
        RLPList(postTxState, cumulativeGasUsed, logsBloomFilter, RLPList(logs.map(_.toRLPEncodable): _*))
      }
    }

    implicit final class ReceiptSeqEnc(receipts: Seq[Receipt]) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = RLPList(receipts.map(_.toRLPEncodable): _*)
    }

    implicit final class ReceiptDec(val bytes: Array[Byte]) {
      def toReceipt: Receipt = ReceiptRLPEncodableDec(rlp.rawDecode(bytes)).toReceipt

      def toReceipts: Seq[Receipt] = rlp.rawDecode(bytes) match {
        case RLPList(items @ _*) => items.map(_.toReceipt)
        case _                   => throw new RuntimeException("Cannot decode Receipts")
      }
    }

    implicit final class ReceiptRLPEncodableDec(val rlpEncodeable: RLPEncodeable) {
      def toReceipt: Receipt = rlpEncodeable match {
        case RLPList(postTxState, cumulativeGasUsed, logsBloomFilter, logs: RLPList) =>
          Receipt(postTxState, cumulativeGasUsed, logsBloomFilter, logs.items.map(_.toTxLogEntry))
        case _ =>
          throw new RuntimeException("Cannot decode Receipt")
      }
    }
  }

}
