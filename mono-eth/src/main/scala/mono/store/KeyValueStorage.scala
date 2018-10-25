package mono.store

import mono.store.datasource.DataSource
import mono.util.SimpleMap

object Namespaces {
  val ReceiptsNamespace = Array[Byte]('r'.toByte)
  val HeaderNamespace = Array[Byte]('h'.toByte)
  val BodyNamespace = Array[Byte]('b'.toByte)
  val NodeNamespace = Array[Byte]('n'.toByte)
  val CodeNamespace = Array[Byte]('c'.toByte)
  val TotalDifficultyNamespace = Array[Byte]('t'.toByte)
  val AppStateNamespace = Array[Byte]('s'.toByte)
  val KnownNodesNamespace = Array[Byte]('k'.toByte)
  val HeightsNamespace = Array[Byte]('i'.toByte)
  val FastSyncStateNamespace = Array[Byte]('h'.toByte)
  val TransactionMappingNamespace = Array[Byte]('l'.toByte)
}

trait KeyValueStorage[K, V, T <: KeyValueStorage[K, V, T]] extends SimpleMap[K, V, T] {

  val source: DataSource
  val namespace: Array[Byte]
  def keySerializer: K => Array[Byte]
  def valueSerializer: V => Array[Byte]
  def valueDeserializer: Array[Byte] => V

  protected def apply(dataSource: DataSource): T

  /**
    * This function obtains the associated value to a key in the current namespace, if there exists one.
    *
    * @param key
    * @return the value associated with the passed key, if there exists one.
    */
  def get(key: K): Option[V] = source.get(namespace, keySerializer(key)).map(valueDeserializer)

  /**
    * This function updates the KeyValueStorage by deleting, updating and inserting new (key-value) pairs
    * in the current namespace.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStorage.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStorage.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new KeyValueStorage after the removals and insertions were done.
    */
  def update(toRemove: Set[K], toUpsert: Map[K, V]): T = {
    val newDataSource = source.update(
      namespace = namespace,
      toRemove = toRemove.map(keySerializer),
      toUpsert = toUpsert.map { case (k, v) => keySerializer(k) -> valueSerializer(v) }
    )
    apply(newDataSource)
  }
}

