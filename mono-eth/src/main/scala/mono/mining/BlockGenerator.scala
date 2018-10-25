package mono.mining

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import java.util.function.UnaryOperator

import akka.actor.ActorSystem
import akka.util.ByteString
import mono.Hash
import mono.domain._
import mono.ledger.{BloomFilter, Ledger}
import mono.ledger.Ledger.{BlockPreparationError, BlockPreparationResult, BlockResult}
import mono.mining.BlockGenerator.{InvalidOmmers, NoParent}
import mono.network.p2p.messages.PV62.BlockBody
import mono.store.datasource.EphemDataSource
import mono.store.trienode.{ArchiveNodeStorage, NodeStorage}
import mono.trie.{ByteArraySerializable, MerklePatriciaTrie}
import mono.util.{BlockchainConfig, BytesUtil, MiningConfig}
import mono.validators.OmmersValidator.OmmersError
import mono.validators.{MptListValidator, Validators}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

class BlockGenerator(
  blockchain:             Blockchain,
  blockchainConfig:       BlockchainConfig,
  miningConfig:           MiningConfig,
  ledger:                 Ledger.I,
  validators:             Validators,
  blockTimestampProvider: BlockTimestampProvider = DefaultBlockTimestampProvider
)(implicit system: ActorSystem) {
  import system.dispatcher

  val difficulty = new DifficultyCalculator(blockchainConfig)
  private val cache: AtomicReference[List[PendingBlock]] = new AtomicReference(Nil)

  def generateBlockForMining(blockNumber: Long, transactions: Seq[SignedTransaction], ommers: Seq[BlockHeader], beneficiary: Address): Either[BlockPreparationError, PendingBlock] = {
    val result = blockchain.getBlockByNumber(blockNumber - 1).map { parent =>
      validators.ommersValidator.validate(blockNumber, ommers, blockchain, Map()) match {
        case Left(error) => Left(InvalidOmmers(error))
        case Right(_) =>
          val blockTimestamp = blockTimestampProvider.getEpochSecond
          val header: BlockHeader = prepareHeader(blockNumber, ommers, beneficiary, parent, blockTimestamp)
          val transactionsForBlock: List[SignedTransaction] = prepareTransactions(transactions, header.gasLimit)

          val body = BlockBody(transactionsForBlock, ommers)
          val block = Block(header, body)
          val prepared = ledger.prepareBlock(block, validators) map {
            case BlockPreparationResult(prepareBlock, BlockResult(_, gasUsed, receipts, parallel, _), stateRoot) =>
              val receiptsLogs: Seq[Array[Byte]] = BloomFilter.emptyBloomFilterBytes +: receipts.map(_.logsBloomFilter.toArray)
              val bloomFilter = ByteString(BytesUtil.or(receiptsLogs: _*))

              Right(PendingBlock(block.copy(
                header = block.header.copy(
                  transactionsRoot = buildMpt(prepareBlock.body.transactionList, SignedTransaction.byteArraySerializable),
                  stateRoot = stateRoot,
                  receiptsRoot = buildMpt(receipts, Receipt.byteArraySerializable),
                  logsBloom = bloomFilter,
                  gasUsed = gasUsed
                ),
                body = prepareBlock.body
              ), receipts))
          }

          Await.result(prepared, Duration.Inf)

      }
    }.getOrElse(Left(NoParent))

    result.right.foreach(b => cache.updateAndGet(new UnaryOperator[List[PendingBlock]] {
      override def apply(t: List[PendingBlock]): List[PendingBlock] =
        (b :: t).take(miningConfig.blockCacheSize)
    }))

    result
  }

  private def prepareHeader(blockNumber: Long, ommers: Seq[BlockHeader], beneficiary: Address, parent: Block, blockTimestamp: Long) = BlockHeader(
    parentHash = parent.header.hash,
    transactionsRoot = Hash(),
    stateRoot = Hash(),
    receiptsRoot = Hash(),
    logsBloom = ByteString.empty,
    difficulty = difficulty.calculateDifficulty(blockNumber, blockTimestamp, parent.header),
    number = blockNumber,
    gasLimit = calculateGasLimit(parent.header.gasLimit),
    gasUsed = 0
  )

  private def prepareTransactions(transactions: Seq[SignedTransaction], blockGasLimit: Long) = {
    val sortedTransactions = transactions.groupBy(_.sender).values.toList.flatMap { txsFromSender =>
      val ordered = txsFromSender.sortBy(-_.tx.gasPrice)
        .sortBy(_.tx.nonce)
        .foldLeft(Vector[SignedTransaction]()) {
          case (txs, tx) =>
            if (txs.exists(_.tx.nonce.compareTo(tx.tx.nonce) == 0)) {
              txs
            } else {
              txs :+ tx
            }
        }.takeWhile(_.tx.gasLimit <= blockGasLimit)
      ordered.headOption.map(_.tx.gasPrice -> ordered)
    }.sortBy { case (gasPrice, _) => gasPrice }.reverse.flatMap { case (_, txs) => txs }

    val transactionsForBlock = sortedTransactions
      .scanLeft(0L, None: Option[SignedTransaction]) { case ((accGas, _), stx) => (accGas + stx.tx.gasLimit, Some(stx)) }
      .collect { case (gas, Some(stx)) => (gas, stx) }
      .takeWhile { case (gas, _) => gas <= blockGasLimit }
      .map { case (_, stx) => stx }
    transactionsForBlock
  }

  //returns maximal limit to be able to include as many transactions as possible
  private def calculateGasLimit(parentGas: Long): Long = {
    val GasLimitBoundDivisor: Int = 1024

    val gasLimitDifference = parentGas / GasLimitBoundDivisor
    parentGas + gasLimitDifference - 1
  }

  private def buildMpt[K](entities: Seq[K], vSerializable: ByteArraySerializable[K]): Hash = {
    val mpt = MerklePatriciaTrie[Int, K](
      source = new ArchiveNodeStorage(new NodeStorage(EphemDataSource()))
    )(MptListValidator.intByteArraySerializable, vSerializable)
    val hash = entities.zipWithIndex.foldLeft(mpt) { case (trie, (value, key)) => trie.put(key, value) }.rootHash
    Hash(hash)
  }

}

trait BlockTimestampProvider {
  def getEpochSecond: Long
}

object DefaultBlockTimestampProvider extends BlockTimestampProvider {
  override def getEpochSecond: Long = Instant.now.getEpochSecond
}

final case class PendingBlock(block: Block, receipts: Seq[Receipt])

object BlockGenerator {
  case object NoParent extends BlockPreparationError
  final case class InvalidOmmers(reason: OmmersError) extends BlockPreparationError
}