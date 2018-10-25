package mono.store.datasource

import akka.util.ByteString


object EphemDataSource {
  def apply(): EphemDataSource = new EphemDataSource(Map())
}

class EphemDataSource(private var storage: Map[ByteString, Array[Byte]]) extends DataSource {
  /**
    * This function obtains the associated value to a key, if there exists one.
    *
    * @param namespace which will be searched for the key.
    * @param key
    * @return the value associated with the passed key.
    */
  override def get(namespace: Namespace, key: Key) = storage.get(ByteString(namespace ++ key))

  /**
    * This function updates the DataSource by deleting, updating and inserting new (key-value) pairs.
    *
    * Note: toRemove and toUpsert should have been processed orderly properly, and so as
    * then, the keys in toRemove are not contained in toUpsert and vice versa. This
    * makes the order of toRemove and toUpsert free
    *
    * @param namespace from which the (key-value) pairs will be removed and inserted.
    * @param toRemove  which includes all the keys to be removed from the DataSource.
    * @param toUpsert  which includes all the (key-value) pairs to be inserted into the DataSource.
    *                  If a key is already in the DataSource its value will be updated.
    * @return the new DataSource after the removals and insertions were done.
    */
  override def update(namespace: Namespace, toRemove: Iterable[Key], toUpsert: Iterable[(Key, Value)]) = {
    val afterRemove = toRemove.foldLeft(storage) { (storage, key) =>
      storage - ByteString(namespace ++ key)
    }
    val afterUpdate = toUpsert.foldLeft(afterRemove) {
      case (storage, (key, value)) =>
        storage + (ByteString(namespace ++ key) -> value)
    }
    storage = afterUpdate
    this
  }

  def toSeq = storage.toSeq.map(x => x._1.toArray -> x._2)
}
