package mono.store

import akka.actor.ActorSystem
import mono.store.datasource.DataSources
import mono.store.trienode.NodeTableStorage

object Storages {

  trait DefaultStorages extends Storages with DataSources {
    lazy val accountNodeStorageFor: (Option[Long]) => NodeTableStorage = bn => new NodeTableStorage(accountNodeDataSource)
    lazy val storageNodeStorageFor: (Option[Long]) => NodeTableStorage = bn => new NodeTableStorage(storageNodeDataSource)


    lazy val appStateStorage = new AppStateStorage(appStateDataSource)
    lazy val blockHeaderStorage = new BlockHeaderStorage(blockHeaderDataSource)
    lazy val blockBodyStorage = new BlockBodyStorage(blockBodyDataSource)


    lazy val transactionMappingStorage = new TransactionMappingStorage(transactionMappingDataSource)
  }

}

trait Storages extends BlockchainStorages {

  implicit protected val system: ActorSystem

}