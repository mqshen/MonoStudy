package mono.store

import kesque.TVal
import mono.Hash
import mono.network.p2p.messages.PV62.BlockBody
import mono.store.datasource.KesqueDataSource
import mono.util.SimpleMap

class BlockBodyStorage(val source: KesqueDataSource) extends SimpleMap[Hash, BlockBody, BlockBodyStorage] {
  import BlockBody.BlockBodyDec


  val namespace: Array[Byte] = Namespaces.BodyNamespace
  def keySerializer: Hash => Array[Byte] = _.bytes
  def valueSerializer: BlockBody => Array[Byte] = _.toBytes
  def valueDeserializer: Array[Byte] => BlockBody = b => b.toBlockBody
  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Set[Hash], toUpsert: Map[Hash, BlockBody]) = {
    toUpsert foreach { case (key, value) => source.put(key, TVal(value.toBytes, -1L)) }
    toRemove foreach { key => source.remove(key) }
    this
  }

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  override def get(key: Hash) = source.get(key).map(_.value.toBlockBody)
}
