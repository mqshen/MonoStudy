package mono.ledger

import akka.actor.ActorSystem
import akka.util.ByteString
import mono.{Hash, UInt256}
import mono.domain._
import mono.util.BlockchainConfig
import mono.validators.Validators
import mono.vm.{ProgramError, ProgramState}

import scala.concurrent.{ExecutionContext, Future}

object Ledger {

  trait I {
    def validateBlocksBeforeExecution(blocks: Seq[Block], validators: Validators)(implicit executor: ExecutionContext): Future[(Vector[Block], Option[BlockExecutionError])]
    def executeBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext): Future[Either[BlockExecutionError, BlockResult]]
    def prepareBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext): Future[BlockPreparationResult]
    def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader)(implicit executor: ExecutionContext): TxResult
  }

  final case class BlockResult(world: BlockWorldState, gasUsed: Long = 0, receipts: Seq[Receipt] = Nil, parallelCount: Int, dbReadTimePercent: Double)
  final case class BlockPreparationResult(block: Block, blockResult: BlockResult, stateRootHash: Hash)
  final case class TxResult(stx: SignedTransaction, world: BlockWorldState, gasUsed: Long, txFee: UInt256, logs: Seq[TxLogEntry], touchedAddresses: Set[Address], vmReturnData: ByteString, error: Option[ProgramError], isRevert: Boolean, parallelRaceConditions: Set[ProgramState.ParallelRace])


  trait BlockPreparationError

  sealed trait BlockExecutionError {
    def blockNumber: Long
    def reason: String
  }

}

final class Ledger(blockchain: Blockchain, blockchainConfig: BlockchainConfig)(implicit system: ActorSystem) extends Ledger.I {
  override def validateBlocksBeforeExecution(blocks: Seq[Block], validators: Validators)(implicit executor: ExecutionContext) = ???

  override def executeBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext) = ???

  override def prepareBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext) = ???

  override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader)(implicit executor: ExecutionContext) = ???
}