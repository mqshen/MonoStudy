package mono.network.p2p.messages

import mono.crypto.ECDSASignature
import mono.domain.{Address, SignedTransaction, Transaction}
import mono.rlp
import mono.rlp.{RLPEncodeable, RLPList, RLPSerializable, RLPValue}
import mono.rlp.RLPImplicitConversions._
import mono.rlp.RLPImplicits._

object CommonMessages {

  object SignedTransactions {

    implicit final class SignedTransactionRlpEncodableDec(val rlpEncodeable: RLPEncodeable) {
      def toSignedTransaction: SignedTransaction = SignedTransactions.toSignedTransaction(rlpEncodeable)
    }

    implicit final class SignedTransactionDec(val bytes: Array[Byte]) {
      def toSignedTransaction: SignedTransaction = rlp.rawDecode(bytes).toSignedTransaction
    }

    implicit final class SignedTransactionEnc(val signedTx: SignedTransaction) extends RLPSerializable {
      override def toRLPEncodable: RLPEncodeable = {
        import signedTx._
        import signedTx.tx._
        RLPList(
          rlp.toRLPEncodable(nonce),
          rlp.toRLPEncodable(gasPrice),
          gasLimit,
          receivingAddress.map(_.toArray).getOrElse(Array.emptyByteArray): Array[Byte],
          rlp.toRLPEncodable(value),
          payload,
          ECDSASignature.getEncodeV(signature.v, chainId).fold(x => x, y => y),
          signature.r,
          signature.s
        )
      }
    }

    def toSignedTransaction(rlpEncodeable: RLPEncodeable): SignedTransaction = rlpEncodeable match {
      case RLPList(nonce, gasPrice, gasLimit, RLPValue(receivingAddress), value, payload, v, r, s) =>
        val receivingAddressOpt = if (receivingAddress.length == 0) None else Some(Address(receivingAddress))
        val realV = ECDSASignature.getRealV(v)
        val chainId = ECDSASignature.extractChainIdFromV(v)

        SignedTransaction(
          Transaction(rlp.toUInt256(nonce), rlp.toUInt256(gasPrice), gasLimit, receivingAddressOpt, rlp.toUInt256(value), payload),
          r,
          s,
          realV,
          chainId
        )
    }

  }

}
