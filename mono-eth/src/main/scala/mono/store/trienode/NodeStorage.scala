package mono.store.trienode

import mono.Hash
import mono.store.{KeyValueStorage, Namespaces}
import mono.store.datasource.DataSource

case class NodeStorage(val source: DataSource) extends KeyValueStorage[Hash, Array[Byte], NodeStorage] {

  val namespace: Array[Byte] = Namespaces.NodeNamespace
  def keySerializer: Hash => Array[Byte] = _.bytes
  def valueSerializer: Array[Byte] => Array[Byte] = identity
  def valueDeserializer: Array[Byte] => Array[Byte] = identity

  protected def apply(dataSource: DataSource): NodeStorage = new NodeStorage(dataSource)
}
