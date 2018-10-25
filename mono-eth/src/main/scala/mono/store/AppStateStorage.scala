package mono.store
import java.nio.ByteBuffer

object AppStateStorage {

  final case class Key private(name: String)

  object Keys {
    val BestBlockNumber = Key("BestBlockNumber")
    val FastSyncDone = Key("FastSyncDone")
    val EstimatedHighestBlock = Key("EstimatedHighestBlock")
    val SyncStartingBlock = Key("SyncStartingBlock")
    val LastPrunedBlock = Key("LastPrunedBlock")
  }

  def longToBytes(v: Long) = ByteBuffer.allocate(8).putLong(v).array
  def bytesToLong(v: Array[Byte]) = ByteBuffer.wrap(v).getLong

}


import AppStateStorage._
import mono.store.datasource.DataSource

class AppStateStorage(val source: DataSource) extends KeyValueStorage[Key, Long, AppStateStorage] {
  override val namespace = Namespaces.AppStateNamespace

  override def keySerializer: Key => Array[Byte] = _.name.getBytes
  override def valueSerializer: Long => Array[Byte] = longToBytes
  override def valueDeserializer: Array[Byte] => Long = bytesToLong

  override protected def apply(dataSource: DataSource): AppStateStorage = new AppStateStorage(dataSource)

  def getBestBlockNumber(): Long =
    get(Keys.BestBlockNumber).getOrElse(0)


}
