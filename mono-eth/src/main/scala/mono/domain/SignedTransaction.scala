package mono.domain

import java.math.BigInteger

import mono.{Hash, crypto, rlp, util}
import mono.crypto.ECDSASignature
import mono.network.p2p.messages.CommonMessages.SignedTransactions._
import mono.rlp.RLPList
import mono.rlp.RLPImplicits._
import mono.rlp.RLPImplicitConversions._
import mono.trie.ByteArraySerializable
import mono.util.Config

object SignedTransaction {
  val FirstByteOfAddress = 12
  val LastByteOfAddress = FirstByteOfAddress + Address.Length

  val ValueForEmptyR = 0
  val ValueForEmptyS = 0

  private def getSender(tx: Transaction, signature: ECDSASignature, chainId: Option[Int]): Option[Address] = {
    // TODO etc always set chainId to that in configuration file
    val rawHash = if (Config.isEth) transactionRawHash_eth(tx, chainId) else transactionRawHash_etc(tx, signature.v, chainId)

    // etc behavior
    //val messageHash = signature.v match {
    //  case ECDSASignature.NegativePointSign | ECDSASignature.PositivePointSign => generalTransactionBytes(tx)
    //  case _ => chainSpecificTransactionBytes(tx, chainId.get)
    //}

    ECDSASignature.recoverPublicKey(signature, rawHash, chainId) flatMap { pubKey =>
      val addrBytes = crypto.kec256(pubKey).slice(FirstByteOfAddress, LastByteOfAddress)
      if (addrBytes.length == Address.Length) {
        Some(Address(addrBytes))
      } else {
        None
      }
    }
  }

  /**
    * For signatures you have to keep also
    * RLP of the transaction without any signature data
    */
  private def transactionRawHash_etc(tx: Transaction, v: Byte, chainId: Option[Int]): Array[Byte] = {
    val rlpValue = v match {
      case ECDSASignature.NegativePointSign | ECDSASignature.PositivePointSign =>
        RLPList(
          rlp.toRLPEncodable(tx.nonce),
          rlp.toRLPEncodable(tx.gasPrice),
          tx.gasLimit,
          tx.receivingAddress.fold(Array.emptyByteArray)(_.toArray),
          rlp.toRLPEncodable(tx.value),
          tx.payload
        )

      case _ =>
        RLPList(
          rlp.toRLPEncodable(tx.nonce),
          rlp.toRLPEncodable(tx.gasPrice),
          tx.gasLimit,
          tx.receivingAddress.fold(Array.emptyByteArray)(_.toArray),
          rlp.toRLPEncodable(tx.value),
          tx.payload,
          chainId.get,
          ValueForEmptyR,
          ValueForEmptyS
        )
    }

    crypto.kec256(rlp.encode(rlpValue))
  }

  /**
    * For signatures you have to keep also
    * RLP of the transaction without any signature data
    */
  private def transactionRawHash_eth(tx: Transaction, chainId: Option[Int]): Array[Byte] = {
    val rlpValue = chainId match {
      case None =>
        RLPList(
          rlp.toRLPEncodable(tx.nonce),
          rlp.toRLPEncodable(tx.gasPrice),
          tx.gasLimit,
          tx.receivingAddress.fold(Array.emptyByteArray)(_.toArray),
          rlp.toRLPEncodable(tx.value),
          tx.payload
        )

      case Some(cid) =>
        RLPList(
          rlp.toRLPEncodable(tx.nonce),
          rlp.toRLPEncodable(tx.gasPrice),
          tx.gasLimit,
          tx.receivingAddress.fold(Array.emptyByteArray)(_.toArray),
          rlp.toRLPEncodable(tx.value),
          tx.payload,
          cid,
          ValueForEmptyR,
          ValueForEmptyS
        )
    }

    crypto.kec256(rlp.encode(rlpValue))
  }

  val byteArraySerializable = new ByteArraySerializable[SignedTransaction] {
    def fromBytes(bytes: Array[Byte]): SignedTransaction = bytes.toSignedTransaction
    def toBytes(input: SignedTransaction): Array[Byte] = input.toBytes
  }

  def apply(tx: Transaction, r: Array[Byte], s: Array[Byte], v: Byte, chainId: Option[Int]): SignedTransaction =
    new SignedTransaction(tx, ECDSASignature(new BigInteger(1, r), new BigInteger(1, s), v), chainId, null)

  def apply(tx: Transaction, signature: ECDSASignature, chainId: Option[Int], sender: Address): SignedTransaction =
    new SignedTransaction(tx, signature, chainId, sender)
}
final class SignedTransaction(
  val tx:              Transaction,
  val signature:       ECDSASignature,
  val chainId:         Option[Int],
  private var _sender: Address
) {

  /**
    * lazy evaulate sender when not set, it's time cost evaulate
    */
  def sender: Address = {
    if (_sender eq null) {
      _sender = SignedTransaction.getSender(tx, signature, chainId) getOrElse (throw new Exception(s"Tx with invalid signature: v=${signature.v}, chainId=$chainId"))
    }
    _sender
  }

  override def toString: String = {
    s"""SignedTransaction {
       |tx: $tx
       |signature: $signature
       |sender: ${sender}
       |chainId: $chainId
       |}""".stripMargin
  }

  def isChainSpecific: Boolean = {
    // TODO
    signature.v match {
      case ECDSASignature.NegativePointSign | ECDSASignature.PositivePointSign => false
      case _ => true
    }
  }

  lazy val hash: Hash = Hash(crypto.kec256(this.toBytes))
}
