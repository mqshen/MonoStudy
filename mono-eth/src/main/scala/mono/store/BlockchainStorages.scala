package mono.store

import mono.store.datasource.KesqueDataSource

trait BlockchainStorages {

  def blockHeaderStorage: BlockHeaderStorage
  def blockBodyStorage: BlockBodyStorage

  def blockHeaderDataSource: KesqueDataSource
  def blockBodyDataSource: KesqueDataSource

}
