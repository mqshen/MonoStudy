package mono.domain

import akka.event.Logging
import akka.util.ByteString
import mono.{Hash, UInt256}
import mono.ledger.{BlockWorldState, TrieStorage}
import mono.network.p2p.messages.PV62.BlockBody
import mono.store.BlockchainStorages
import mono.vm.{Storage, WorldState}

object Blockchain {
  trait I[S <: Storage[S], W <: WorldState[W, S]] {
    /**
      * Allows to query a blockHeader by block hash
      *
      * @param hash of the block that's being searched
      * @return [[BlockHeader]] if found
      */
    def getBlockHeaderByHash(hash: Hash): Option[BlockHeader]

    def getBlockHeaderByNumber(number: Long): Option[BlockHeader] = {
      for {
        hash <- getHashByBlockNumber(number)
        header <- getBlockHeaderByHash(hash)
      } yield header
    }

    /**
      * Allows to query for a block based on it's number
      *
      * @param number Block number
      * @return Block if it exists
      */
    def getBlockByNumber(number: Long): Option[Block] =
      for {
        hash <- getHashByBlockNumber(number)
        block <- getBlockByHash(hash)
      } yield block

    /**
      * Allows to query a blockBody by block hash
      *
      * @param hash of the block that's being searched
      * @return [[mono.network.p2p.messages.PV62.BlockBody]] if found
      */
    def getBlockBodyByHash(hash: Hash): Option[BlockBody]

    /**
      * Returns a block hash given a block number
      *
      * @param number Number of the searchead block
      * @return Block hash if found
      */
    def getHashByBlockNumber(number: Long): Option[Hash]

    /**
      * Allows to query for a block based on it's hash
      *
      * @param hash of the block that's being searched
      * @return Block if found
      */
    def getBlockByHash(hash: Hash): Option[Block] =
      for {
        header <- getBlockHeaderByHash(hash)
        body <- getBlockBodyByHash(hash)
      } yield Block(header, body)

    /**
      * Persists a block in the underlying Blockchain Database
      *
      * @param block Block to be saved
      */
    def saveBlock(block: Block): Unit = {
      saveBlockHeader(block.header)
      saveBlockBody(block.header.hash, block.body)
    }

    /**
      * Persists a block header in the underlying Blockchain Database
      *
      * @param blockHeader Block to be saved
      */
    def saveBlockHeader(blockHeader: BlockHeader): Unit
    def saveBlockBody(blockHash: Hash, blockBody: BlockBody): Unit
    def saveReceipts(blockHash: Hash, receipts: Seq[Receipt]): Unit
    def saveEvmcode(hash: Hash, evmCode: ByteString): Unit
    def saveTotalDifficulty(blockhash: Hash, totalDifficulty: UInt256): Unit
  }


  def apply(storages: BlockchainStorages): Blockchain =
    new Blockchain(storages)
}
class Blockchain(val storages: BlockchainStorages) extends Blockchain.I[TrieStorage, BlockWorldState] {
  private val blockHeaderStorage = storages.blockHeaderStorage
  private val blockBodyStorage = storages.blockBodyStorage
  /**
    * Allows to query a blockHeader by block hash
    *
    * @param hash of the block that's being searched
    * @return [[BlockHeader]] if found
    */
  override def getBlockHeaderByHash(hash: Hash) = ???

  /**
    * Allows to query a blockBody by block hash
    *
    * @param hash of the block that's being searched
    * @return [[mono.network.p2p.messages.PV62.BlockBody]] if found
    */
  override def getBlockBodyByHash(hash: Hash) = ???

  /**
    * Returns a block hash given a block number
    *
    * @param number Number of the searchead block
    * @return Block hash if found
    */
  override def getHashByBlockNumber(number: Long) = {
    val t = blockHeaderStorage.getBlockHash(number)
    t
  }

  /**
    * Persists a block header in the underlying Blockchain Database
    *
    * @param blockHeader Block to be saved
    */
  override def saveBlockHeader(blockHeader: BlockHeader): Unit = {
    val hash = blockHeader.hash
    blockHeaderStorage.setWritingBlockNumber(blockHeader.number)
    blockHeaderStorage.put(hash, blockHeader)
    saveBlockNumberMapping(blockHeader.number, hash)
  }

  override def saveBlockBody(blockHash: Hash, blockBody: BlockBody): Unit = {
    blockBodyStorage.put(blockHash, blockBody)
    saveTxsLocations(blockHash, blockBody)
  }

  override def saveReceipts(blockHash: Hash, receipts: Seq[Receipt]): Unit = {
    //TODO
    println("need to implement saveReceipts")
  }

  override def saveEvmcode(hash: Hash, evmCode: ByteString): Unit = {
    //TODO
    println("need to implement saveEvmcode")
  }

  override def saveTotalDifficulty(blockhash: Hash, totalDifficulty: UInt256): Unit = {
    //TODO
    println("need to implement saveTotalDifficulty ")
  }

  private def saveTxsLocations(blockHash: Hash, blockBody: BlockBody): Unit = {
    //TODO
    println("need to implement saveTotalDifficulty ")
  }

  private def saveBlockNumberMapping(number: Long, hash: Hash): Unit =
    blockHeaderStorage.putBlockHash(number, hash)
}
