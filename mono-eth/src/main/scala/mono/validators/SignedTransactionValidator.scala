package mono.validators

import mono.UInt256
import mono.domain.{Account, BlockHeader, SignedTransaction}
import mono.util.BlockchainConfig

sealed trait SignedTransactionError
object SignedTransactionError {
  case object TransactionSignatureError extends SignedTransactionError
  final case class TransactionSyntaxError(reason: String) extends SignedTransactionError
  final case class TransactionNonceError(txNonce: UInt256, senderNonce: UInt256) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Got tx nonce $txNonce but sender in mpt is: $senderNonce)"
  }
  final case class TransactionNotEnoughGasForIntrinsicError(txGasLimit: Long, txIntrinsicGas: Long) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Tx gas limit ($txGasLimit) < tx intrinsic gas ($txIntrinsicGas))"
  }
  final case class TransactionSenderCantPayUpfrontCostError(upfrontCost: UInt256, senderBalance: UInt256) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Upfrontcost ($upfrontCost) > sender balance ($senderBalance))"
  }
  final case class TransactionGasLimitTooBigError(txGasLimit: Long, accumGasUsed: Long, blockGasLimit: Long) extends SignedTransactionError {
    override def toString: String =
      s"${getClass.getSimpleName}(Tx gas limit ($txGasLimit) + gas accum ($accumGasUsed) > block gas limit ($blockGasLimit))"
  }
}

final class SignedTransactionValidator(blockchainConfig: BlockchainConfig) {


  /**
    * Initial tests of intrinsic validity stated in Section 6 of YP
    *
    * @param stx                        Transaction to validate
    * @param senderAccount              Account of the sender of the tx
    * @param blockHeader                Container block
    * @param upfrontGasCost    The upfront gas cost of the tx
    * @param accumGasUsed               Total amount of gas spent prior this transaction within the container block
    * @return Transaction if valid, error otherwise
    */
  def validate(stx: SignedTransaction, senderAccount: Account, blockHeader: BlockHeader, upfrontGasCost: UInt256, accumGasUsed: Long): Either[SignedTransactionError, Unit] = {
//    for {
//      _ <- checkSyntacticValidity(stx)
//      _ <- validateSignature(stx, blockHeader.number)
//      _ <- validateNonce(stx, senderAccount.nonce)
//      _ <- validateGasLimitEnoughForIntrinsicGas(stx, blockHeader.number)
//      _ <- validateAccountHasEnoughGasToPayUpfrontCost(senderAccount.balance, upfrontGasCost)
//      _ <- validateBlockHasEnoughGasLimitForTx(stx, accumGasUsed, blockHeader.gasLimit)
//    } yield ()
    Right(())
  }

}
