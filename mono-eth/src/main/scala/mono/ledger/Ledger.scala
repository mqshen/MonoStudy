package mono.ledger

import akka.actor.ActorSystem
import akka.event.Logging
import akka.util.ByteString
import mono.{Hash, UInt256}
import mono.domain._
import mono.ledger.Ledger._
import mono.trie.MerklePatriciaTrie.MPTNodeMissingException
import mono.util.BlockchainConfig
import mono.validators.{SignedTransactionError, SignedTransactionValidator, Validators}
import mono.vm._

import scala.concurrent.{ExecutionContext, Future}

object Ledger {

  trait I {
    def validateBlocksBeforeExecution(blocks: Seq[Block], validators: Validators)(implicit executor: ExecutionContext): Future[(Vector[Block], Option[BlockExecutionError])]
    def executeBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext): Future[Either[BlockExecutionError, BlockResult]]
    def prepareBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext): Future[BlockPreparationResult]
    def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader)(implicit executor: ExecutionContext): TxResult
  }

  type PC = ProgramContext[BlockWorldState, TrieStorage]
  type PR = ProgramResult[BlockWorldState, TrieStorage]

  final case class BlockResult(world: BlockWorldState, gasUsed: Long = 0, receipts: Seq[Receipt] = Nil, parallelCount: Int, dbReadTimePercent: Double)
  final case class BlockPreparationResult(block: Block, blockResult: BlockResult, stateRootHash: Hash)
  final case class TxResult(stx: SignedTransaction, world: BlockWorldState, gasUsed: Long, txFee: UInt256, logs: Seq[TxLogEntry], touchedAddresses: Set[Address], vmReturnData: ByteString, error: Option[ProgramError], isRevert: Boolean, parallelRaceConditions: Set[ProgramState.ParallelRace])

  final case class TxsExecutionError(blockNumber: Long, stx: SignedTransaction, stateBeforeError: StateBeforeFailure, error: SignedTransactionError) extends BlockExecutionError { def reason = error.toString }
  final case class MissingNodeExecptionError(blockNumber: Long, hash: Hash, table: String) extends BlockExecutionError { def reason = s"Missing node $hash in $table" }
  final case class StateBeforeFailure(blockNumber: Long, worldState: BlockWorldState, cumGas: Long, cumReceipts: Vector[Receipt])

  trait BlockPreparationError
  sealed trait BlockExecutionError {
    def blockNumber: Long
    def reason: String
  }

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price) + Tv (Tx value). See YP equation number (65)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private def calculateUpfrontCost(tx: Transaction): UInt256 =
    calculateUpfrontGas(tx) + tx.value

  /**
    * v0 ≡ Tg (Tx gas limit) * Tp (Tx gas price). See YP equation number (68)
    *
    * @param tx Target transaction
    * @return Upfront cost
    */
  private def calculateUpfrontGas(tx: Transaction): UInt256 =
    UInt256(tx.gasLimit) * tx.gasPrice

}

final class Ledger(blockchain: Blockchain, blockchainConfig: BlockchainConfig)(implicit system: ActorSystem) extends Ledger.I {
  private val log = Logging(system, this.getClass)

  override def validateBlocksBeforeExecution(blocks: Seq[Block], validators: Validators)(implicit executor: ExecutionContext) = ???

  override def executeBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext) = ???

  override def prepareBlock(block: Block, validators: Validators)(implicit executor: ExecutionContext) = {
    val parentStateRoot = blockchain.getBlockHeaderByHash(block.header.parentHash).map(_.stateRoot)
    val initialWorld = blockchain.getReadOnlyWorldState(None, blockchainConfig.accountStartNonce, parentStateRoot)

    executePreparedTransactions(block.body.transactionList, initialWorld, block.header, validators.signedTransactionValidator) map {
      case (execResult @ BlockResult(resultingWorldState, _, _, _, _), txExecuted) =>
        val worldRewardPaid = payBlockReward(block)(resultingWorldState)
        val worldPersisted = worldRewardPaid.commit().persist()
        BlockPreparationResult(block.copy(body = block.body.copy(transactionList = txExecuted)), execResult, worldPersisted.stateRootHash)
    }
  }

  private def executePreparedTransactions(
    signedTransactions:         Seq[SignedTransaction],
    world:                      BlockWorldState,
    blockHeader:                BlockHeader,
    signedTransactionValidator: SignedTransactionValidator,
    accGas:                     Long                       = 0,
    accReceipts:                Vector[Receipt]            = Vector(),
    executed:                   Vector[SignedTransaction]  = Vector()
  )(implicit executor: ExecutionContext): Future[(BlockResult, Vector[SignedTransaction])] = {
    val evmCfg = EvmConfig.forBlock(blockHeader.number, blockchainConfig)
    executeTransactions_sequential(signedTransactions, blockHeader, signedTransactionValidator, evmCfg)(world) flatMap {
      case Right(br) => Future.successful(br, executed ++ signedTransactions)

      case Left(TxsExecutionError(blockHeader.number, stx, StateBeforeFailure(blockHeader.number, worldState, gas, receipts), reason)) =>
        log.debug(s"failure while preparing block because of $reason in transaction with hash ${stx.hash}")
        val txIndex = signedTransactions.indexWhere(tx => tx.hash == stx.hash)
        executePreparedTransactions(
          signedTransactions.drop(txIndex + 1),
          worldState, blockHeader, signedTransactionValidator, gas, receipts, executed ++ signedTransactions.take(txIndex)
        )

      case Left(error) =>
        throw new RuntimeException(s"Error during executePreparedTransactions: $error")
    }
  }

  private def executeTransactions_sequential(
    signedTransactions: Seq[SignedTransaction],
    blockHeader:        BlockHeader,
    stxValidator:       SignedTransactionValidator,
    evmCfg:             EvmConfig
  )(initialWorld: BlockWorldState): Future[Either[BlockExecutionError, BlockResult]] = {
    var currWorld = initialWorld
    var txError: Option[BlockExecutionError] = None
    var txResults = Vector[TxResult]()

    val itr = signedTransactions.iterator
    while (itr.hasNext && txError.isEmpty) {
      val stx = itr.next()
      validateAndExecuteTransaction(stx, blockHeader, stxValidator, evmCfg)(currWorld.withTx(Some(stx))) match {
        case Right(txResult) =>
          currWorld = txResult.world
          txResults = txResults :+ txResult
        case Left(error) =>
          txError = Some(error)
      }
    }

    txError match {
      case Some(error) => Future.successful(Left(error))
      case None        => Future.successful(postExecuteTransactions(blockHeader, evmCfg, txResults, 0, 0.0)(currWorld.withTx(None)))
    }
  }

  /**
    * This function updates state in order to pay rewards based on YP section 11.3
    *
    * @param block
    * @param world
    * @return
    */
  private def payBlockReward(block: Block)(world: BlockWorldState): BlockWorldState = {
    println("need to implement payBlockReward")
    world
    /*
    val minerAddress = Address(block.header.beneficiary)
    val minerAccount = getAccountToPay(minerAddress)(world)
    val minerReward = blockRewardCalculator.calcBlockMinerReward(block.header.number, block.body.uncleNodesList.size)
    val afterMinerReward = world.saveAccount(minerAddress, minerAccount.increaseBalance(minerReward))
    log.debug(s"Paying block ${block.header.number} reward of $minerReward to miner with account address $minerAddress")

    block.body.uncleNodesList.foldLeft(afterMinerReward) { (ws, ommer) =>
      val ommerAddress = Address(ommer.beneficiary)
      val account = getAccountToPay(ommerAddress)(ws)
      val ommerReward = blockRewardCalculator.calcOmmerMinerReward(block.header.number, ommer.number)
      log.debug(s"Paying block ${block.header.number} reward of $ommerReward to ommer with account address $ommerAddress")
      ws.saveAccount(ommerAddress, account.increaseBalance(ommerReward))
    }
    */
  }

  // TODO see TODO at lines
  private[ledger] def validateAndExecuteTransaction(
    stx:          SignedTransaction,
    blockHeader:  BlockHeader,
    stxValidator: SignedTransactionValidator,
    evmCfg:       EvmConfig
  )(world: BlockWorldState): Either[BlockExecutionError, TxResult] = {
    try {
      val (senderAccount, worldForTx) = world.getAccount(stx.sender) match {
        case Some(account) => (account, world)
        case None =>
          val emptyAccount = world.emptyAccount
          (emptyAccount, world.saveAccount(stx.sender, emptyAccount))
      }

      val upfrontCost = calculateUpfrontCost(stx.tx)

      stxValidator.validate(stx, senderAccount, blockHeader, upfrontCost, accumGasUsed = 0L) match { // TODO validate accumGasUsed lazily for asyn execution
        case Right(_) | Left(SignedTransactionError.TransactionNonceError(_, _)) => // TODO validate TransactionNonceError lazily for async execution
          Right(executeTransaction(stx, blockHeader, evmCfg)(worldForTx))

        case Left(error) =>
          Left(TxsExecutionError(blockHeader.number, stx, StateBeforeFailure(blockHeader.number, world, 0L, Vector()), error)) // TODO content of StateBeforeFailure
      }
    } catch {
      case MPTNodeMissingException(_, hash, table) => Left(MissingNodeExecptionError(blockHeader.number, hash, table))
      case e: Throwable                            => throw e
    }
  }

  private def postExecuteTransactions(
                                       blockHeader:   BlockHeader,
                                       evmCfg:        EvmConfig,
                                       txResults:     Vector[TxResult],
                                       parallelCount: Int,
                                       dbTimePercent: Double
                                     )(world: BlockWorldState): Either[BlockExecutionError, BlockResult] = {
    try {
      val (accGas, accTxFee, accTouchedAddresses, accReceipts) = txResults.foldLeft(0L, UInt256.Zero, Set[Address](), Vector[Receipt]()) {
        case ((accGas, accTxFee, accTouchedAddresses, accReceipts), TxResult(stx, worldAfterTx, gasUsed, txFee, logs, touchedAddresses, _, error, isRevert, _)) =>

          val postTxState = Receipt.Success

          log.debug(s"Tx ${stx.hash} gasLimit: ${stx.tx.gasLimit}, gasUsed: $gasUsed, cumGasUsed: ${accGas + gasUsed}")

          val receipt = Receipt(
            postTxState = postTxState,
            cumulativeGasUsed = accGas + gasUsed,
            logsBloomFilter = BloomFilter.create(logs),
            logs = logs
          )

          (accGas + gasUsed, accTxFee + txFee, accTouchedAddresses ++ touchedAddresses, accReceipts :+ receipt)
      }

//      val minerAddress = Address(blockHeader.beneficiary)
//      val worldPayMinerForGas = world.pay(minerAddress, accTxFee)

      val worldPayMinerForGas = 0
      // find empty touched accounts to be deleted
      val deadAccounts = Set[Address]()
      //log.debug(s"touched accounts: ${result.addressesTouched}, miner: $minerAddress")
      log.debug(s"dead accounts accounts: $deadAccounts")
      //val worldDeletedDeadAccounts = deleteAccounts(deadAccounts)(worldPayMinerForGas)
      val worldDeletedDeadAccounts = deleteAccounts(deadAccounts)(null)

      log.debug(s"$blockHeader, accGas $accGas, receipts = $accReceipts")
      Right(BlockResult(worldDeletedDeadAccounts, accGas, accReceipts, parallelCount, dbTimePercent))
    } catch {
      case MPTNodeMissingException(_, hash, table) => Left(MissingNodeExecptionError(blockHeader.number, hash, table))
      case e: Throwable                            => throw e
    }
  }

  private def executeTransaction(
    stx:         SignedTransaction,
    blockHeader: BlockHeader,
    evmCfg:      EvmConfig
  )(world: BlockWorldState): TxResult = {
    val start = System.currentTimeMillis

    // TODO catch prepareProgramContext's throwable (MPTException etc from mtp) here
    val (checkpoint, context) = prepareProgramContext(stx, blockHeader, evmCfg)(world)

//    if (blockchainConfig.isDebugTraceEnabled) {
//      println(s"\nTx 0x${stx.hash} ========>")
//    }
    val result = runVM(stx, context, evmCfg)(checkpoint)

    val gasLimit = stx.tx.gasLimit
    val totalGasToRefund = calcTotalGasToRefund(gasLimit, result)
    val gasUsed = stx.tx.gasLimit - totalGasToRefund
    val gasPrice = stx.tx.gasPrice
    val txFee = gasPrice * gasUsed
    val refund = gasPrice * totalGasToRefund

//    if (blockchainConfig.isDebugTraceEnabled) {
//      println(s"\nTx 0x${stx.hash} gasLimit: ${stx.tx.gasLimit} gasUsed $gasUsed, isRevert: ${result.isRevert}, error: ${result.error}")
//    }

    val worldRefundGasPaid = result.world.pay(stx.sender, refund)
    val worldDeletedAccounts = deleteAccounts(result.addressesToDelete)(worldRefundGasPaid)

    //log.debug(
    //  s"""Transaction 0x${stx.hashAsHexString} execution end. Summary:
    //     | - Value: ${stx.tx.value}
    //     | - Error: ${result.error}.
    //     | - Total Gas to Refund: $totalGasToRefund
    //     | - Execution gas paid to miner: $txFee""".stripMargin
    //)

    val elapsed = System.currentTimeMillis - start
    null
    //TxResult(stx, worldDeletedAccounts, gasUsed, txFee)
  }

  /**
    * @param checkpoint - world will return checkpoint if result error or isRevert
    */
  private def runVM(stx: SignedTransaction, context: PC, evmCfg: EvmConfig)(checkpoint: BlockWorldState): PR = {
    val r = if (stx.tx.isContractCreation) { // create
      VM.run(context, blockchainConfig.isDebugTraceEnabled)
    } else { // call
      PrecompiledContracts.getContractForAddress(context.targetAddress, evmCfg) match {
        case Some(contract) =>
          contract.run(context)
        case None =>
          VM.run(context, blockchainConfig.isDebugTraceEnabled)
      }
    }

    val result = if (stx.tx.isContractCreation && !r.error.isDefined && !r.isRevert) {
      saveCreatedContract(context.env.ownerAddr, r, evmCfg)
    } else {
      r
    }

    if (result.error.isDefined || result.isRevert) {
      // rollback to the world before transfer was done if an error happened
      // the error result may be caused by parallel conflict, so merge all possible modifies
      result.copy(world = checkpoint.mergeRaceConditions(result.world), addressesToDelete = Set(), addressesTouched = Set(), txLogs = Vector(), parallelRaceConditions = Set(ProgramState.OnError))
    } else {
      result
    }
  }

  private def saveCreatedContract(address: Address, result: PR, evmCfg: EvmConfig): PR = {
    val codeDepositCost = evmCfg.calcCodeDepositCost(result.returnData)

    if (result.gasRemaining < codeDepositCost) {
      if (evmCfg.exceptionalFailedCodeDeposit) {
        // TODO set returnData to empty bytes ByteString()?
        result.copy(error = Some(OutOfGas))
      } else {
        result
      }
    } else if (result.returnData.length > evmCfg.maxContractSize) {
      // contract size too large
      log.warning(s"Contract size too large: ${result.returnData.length}")
      // TODO set returnData to empty bytes ByteString()?
      result.copy(error = Some(OutOfGas))
    } else {
      // even result.isRevert? this is a different behavior from CREATE opcode?
      result.copy(
        gasRemaining = result.gasRemaining - codeDepositCost,
        world = result.world.saveCode(address, result.returnData)
      )
    }
  }

  /**
    * Increments account nonce by 1 stated in YP equation (69) and
    * Pays the upfront Tx gas calculated as TxGasPrice * TxGasLimit from balance. YP equation (68)
    * remember the checkpoint world state
    * prepareProgtamContext
    * Note we use one fewer than the sender’s nonce
    * value; we assert that we have incremented the sender account’s
    * nonce prior to this call, and so the value used
    * is the sender’s nonce at the beginning of the responsible
    * transaction or VM operation
    */
  private def prepareProgramContext(stx: SignedTransaction, blockHeader: BlockHeader, evmCfg: EvmConfig)(world: BlockWorldState): (BlockWorldState, PC) = {
    val senderAddress = stx.sender
    val account = world.getGuaranteedAccount(senderAddress)
    val (checkpoint, worldAtCheckpoint) = {
      val worldx = world.withdraw(senderAddress, calculateUpfrontGas(stx.tx)).increaseNonce(senderAddress)
      (worldx.copy, worldx)
    }

    val (worldBeforeTransfer, recipientAddress, program) = if (stx.tx.isContractCreation) {
      val newContractAddress = worldAtCheckpoint.createAddress(senderAddress)
//      val world = if (evmCfg.eip161) {
//        worldAtCheckpoint.increaseNonce(newContractAddress)
//      } else {
        worldAtCheckpoint
//      }
      log.debug(s"newContractAddress: $newContractAddress")
      (world, newContractAddress, Program(stx.tx.payload.toArray))
    } else {
      val txReceivingAddress = stx.tx.receivingAddress.get
      log.debug(s"txReceivingAddress: $txReceivingAddress")
      (worldAtCheckpoint, txReceivingAddress, Program(world.getCode(txReceivingAddress).toArray))
    }

    val worldAfterTransfer = worldBeforeTransfer.transfer(senderAddress, recipientAddress, stx.tx.value)
    val initialAddressesToDelete = Set[Address]()
    val initialAddressesTouched = Set(recipientAddress)

    val context: PC = null
//    ProgramContext(
//      stx,
//      recipientAddress,
//      program,
//      blockHeader,
//      worldAfterTransfer,
//      evmCfg,
//      initialAddressesToDelete,
//      initialAddressesTouched
//      //, isStaticCall = false
//    )

    (checkpoint, context)
  }

  override def simulateTransaction(stx: SignedTransaction, blockHeader: BlockHeader)(implicit executor: ExecutionContext) = ???

  /**
    * Delete all accounts (that appear in SUICIDE list). YP eq (78).
    * The contract storage should be cleared during pruning as nodes could be used in other tries.
    * The contract code is also not deleted as there can be contracts with the exact same code, making it risky to delete
    * the code of an account in case it is shared with another one.
    * FIXME: [EC-242]
    *   Should we delete the storage associated with the deleted accounts?
    *   Should we keep track of duplicated contracts for deletion?
    *
    * @param addressesToDelete
    * @param worldState
    * @return a worldState equal worldState except that the accounts from addressesToDelete are deleted
    */
  private def deleteAccounts(addressesToDelete: Set[Address])(worldState: BlockWorldState): BlockWorldState = {
    addressesToDelete.foldLeft(worldState) { case (world, address) => world.deleteAccount(address) }
  }

  /**
    * Calculate total gas to be refunded
    * See YP, eq (72)
    */
  private def calcTotalGasToRefund(gasLimit: Long, result: PR): Long = {
    if (result.error.isDefined) {
      0
    } else {
      if (result.isRevert) {
        result.gasRemaining
      } else {
        val gasUsed = gasLimit - result.gasRemaining
        result.gasRemaining + math.min(gasUsed / 2, result.gasRefund)
      }
    }
  }
}