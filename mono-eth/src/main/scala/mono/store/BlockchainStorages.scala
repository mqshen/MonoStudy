package mono.store

import mono.store.datasource.KesqueDataSource
import mono.store.trienode.NodeKeyValueStorage

trait BlockchainStorages {
  def accountNodeDataSource: KesqueDataSource
  def storageNodeDataSource: KesqueDataSource

  def blockHeaderStorage: BlockHeaderStorage
  def blockBodyStorage: BlockBodyStorage
  def transactionMappingStorage: TransactionMappingStorage

  def blockHeaderDataSource: KesqueDataSource
  def blockBodyDataSource: KesqueDataSource

  def accountNodeStorageFor: (Option[Long]) => NodeKeyValueStorage
  def storageNodeStorageFor: (Option[Long]) => NodeKeyValueStorage
}
