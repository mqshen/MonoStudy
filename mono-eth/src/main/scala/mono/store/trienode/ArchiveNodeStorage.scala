package mono.store.trienode
import mono.Hash

/**
  * This class is used to store Nodes (defined in mpt/Node.scala), by using:
  * Key: hash of the RLP encoded node
  * Value: the RLP encoded node
  */
final class ArchiveNodeStorage(source: NodeStorage) extends NodeKeyValueStorage {
  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Set[Hash], toUpsert: Map[Hash, Array[Byte]]) = {
    source.update(Set(), toUpsert)
    this
  }

  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  override def get(key: Hash) = source.get(key)
}
