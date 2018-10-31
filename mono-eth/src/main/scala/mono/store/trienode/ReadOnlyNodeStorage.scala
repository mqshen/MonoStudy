package mono.store.trienode

import mono.Hash

object ReadOnlyNodeStorage {
  def apply(source: NodeKeyValueStorage): ReadOnlyNodeStorage =
    new ReadOnlyNodeStorage(source, Map())
}
final class ReadOnlyNodeStorage private (source: NodeKeyValueStorage, cache: Map[Hash, Option[Array[Byte]]]) extends NodeKeyValueStorage {
  /**
    * This function obtains the value asociated with the key passed, if there exists one.
    *
    * @param key
    * @return Option object with value if there exists one.
    */
  override def get(key: Hash) = cache.getOrElse(key, source.get(key))

  /**
    * This function updates the KeyValueStore by deleting, updating and inserting new (key-value) pairs.
    *
    * @param toRemove which includes all the keys to be removed from the KeyValueStore.
    * @param toUpsert which includes all the (key-value) pairs to be inserted into the KeyValueStore.
    *                 If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(toRemove: Set[Hash], toUpsert: Map[Hash, Array[Byte]]) = {
    val afterRemove = toRemove.foldLeft(cache) { (updated, k) => updated + (k -> None) }
    val afterUpsert = toUpsert.foldLeft(afterRemove) { case (updated, (k, v)) => updated + (k -> Some(v)) }
    new ReadOnlyNodeStorage(source, afterUpsert)
  }
}
